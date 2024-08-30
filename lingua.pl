% ---------------------------------
% The lingua reasoner -- Jos De Roo
% ---------------------------------
%
% See https://github.com/eyereasoner/lingua
%

:- use_module(library(lists)).
:- use_module(library(gensym)).
:- use_module(library(system)).
:- use_module(library(terms)).
:- use_module(library(url)).
:- use_module(library(charsio)).
:- use_module(library(base64)).
:- use_module(library(date)).
:- use_module(library(sha)).
:- use_module(library(dif)).
:- use_module(library(pcre)).
:- use_module(library(semweb/turtle)).
:- catch(use_module(library(http/http_open)), _, true).

version_info('lingua v1.3.1').

help_info('Usage: lingua <options>* <data>*

<options>
    --genid <genid>             use <genid> in Skolem IRIs
    --help, -h                  show help info
    --output <file>             write reasoner output to <file>
    --version, -v               show version info
    --wcache <uri> <file>       to tell that <uri> is cached as <file>

<data>
    <uri>                       TriG data').

:- dynamic(answer/3).           % answer(Predicate, Subject, Object)
:- dynamic(apfx/2).
:- dynamic(base_uri/1).
:- dynamic(brake/0).
:- dynamic(cc/1).
:- dynamic(cpred/1).
:- dynamic(exopred/3).          % exopred(Predicate, Subject, Object)
:- dynamic(flag/2).
:- dynamic(fpred/1).
:- dynamic(graph/2).
:- dynamic(hash_value/2).
:- dynamic(implies/2).          % implies(Premise, Conclusion)
:- dynamic(keep_ng/1).
:- dynamic(keep_skolem/1).
:- dynamic(mtime/2).
:- dynamic(ncllit/0).
:- dynamic(ns/2).
:- dynamic(pfx/2).
:- dynamic(pred/1).
:- dynamic(quad/4).
:- dynamic(recursion/1).
:- dynamic(rule_uvar/1).
:- dynamic(scope/1).
:- dynamic(tuple/2).
:- dynamic(tuple/3).
:- dynamic(tuple/4).
:- dynamic(tuple/5).
:- dynamic(tuple/6).
:- dynamic(tuple/7).
:- dynamic(tuple/8).
:- dynamic(unify/2).
:- dynamic(uuid/2).
:- dynamic(wcache/2).
:- dynamic(wpfx/1).
:- dynamic(wtcache/2).
:- dynamic('<http://www.w3.org/1999/02/22-rdf-syntax-ns#first>'/2).
:- dynamic('<http://www.w3.org/1999/02/22-rdf-syntax-ns#rest>'/2).
:- dynamic('<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'/2).
:- dynamic('<http://www.w3.org/1999/02/22-rdf-syntax-ns#value>'/2).
:- dynamic('<http://www.w3.org/2000/01/rdf-schema#subClassOf>'/2).
:- dynamic('<http://www.w3.org/2000/10/swap/log#callWithCleanup>'/2).
:- dynamic('<http://www.w3.org/2000/10/swap/log#collectAllIn>'/2).
:- dynamic('<http://www.w3.org/2000/10/swap/log#explanation>'/2).
:- dynamic('<http://www.w3.org/2000/10/swap/log#implies>'/2).
:- dynamic('<http://www.w3.org/2000/10/swap/log#isImpliedBy>'/2).
:- dynamic('<http://www.w3.org/2000/10/swap/log#query>'/2).

%
% Main goal
%

main(Argv) :-
    set_prolog_flag(argv, Argv),
    catch(run, Exc,
        (   Exc = halt(_)
        ->  true
        ;   throw(Exc)
        )
    ).

main :-
    catch(run, Exc,
        (   Exc = halt(EC)
        ->  halt(EC)
        ;   throw(Exc)
        )
    ).

run :-
    nb_setval(fm, 0),
    nb_setval(mf, 0),
    catch(set_stream(user_output, encoding(utf8)), _, true),
    current_prolog_flag(argv, Argv),
    (   append(_, ['--'|Argvp], Argv)
    ->  true
    ;   Argvp = Argv
    ),
    argv(Argvp, Argus),
    catch(gre(Argus), Exc,
        (   Exc = halt(0)
        ->  true
        ;   format(user_error, '** ERROR ** gre ** ~w~n', [Exc]),
            flush_output(user_error),
            nb_setval(exit_code, 3)
        )
    ),
    nb_getval(fm, Fm),
    (   Fm = 0
    ->  true
    ;   format(user_error, '*** fm=~w~n', [Fm]),
        flush_output(user_error)
    ),
    nb_getval(mf, Mf),
    (   Mf = 0
    ->  true
    ;   format(user_error, '*** mf=~w~n', [Mf]),
        flush_output(user_error)
    ),
    nb_getval(exit_code, EC),
    flush_output,
    throw(halt(EC)).

argv([], []) :-
    !.
argv([Arg|Argvs], [U, V|Argus]) :-
    sub_atom(Arg, B, 1, E, '='),
    sub_atom(Arg, 0, B, _, U),
    memberchk(U, ['--output', '--genid']),
    !,
    sub_atom(Arg, _, E, 0, V),
    argv(Argvs, Argus).
argv([Arg|Argvs], [Arg|Argus]) :-
    argv(Argvs, Argus).


% ------------------------------
% GRE (Generic Reasoning Engine)
% ------------------------------

gre(Argus) :-

    % prepare engine
    nb_setval(exit_code, 0),
    nb_setval(indentation, 0),
    nb_setval(limit, -1),
    nb_setval(tuple, -1),
    nb_setval(fdepth, 0),
    nb_setval(pdepth, 0),
    nb_setval(cdepth, 0),
    nb_setval(wn, 0),
    nb_setval(doc_nr, 0),

    % check command line options
    opts(Argus, Args),
    (   Args = []
    ->  opts(['--help'], _)
    ;   true
    ),

    % get genid for Skolem IRIs
    (   flag(genid, Genid)
    ->  true
    ;   uuid(Genid)
    ),
    atomic_list_concat(['http://eyereasoner.github.io/.well-known/genid/', Genid, '#'], Sns),
    nb_setval(var_ns, Sns),

    % common namespace prefixes
    put_pfx('skolem', Sns),
    put_pfx('var', 'http://www.w3.org/2000/10/swap/var#'),
    put_pfx('rdf', 'http://www.w3.org/1999/02/22-rdf-syntax-ns#'),

    % process command line arguments
    args(Args),

    % create named graphs
    (   quad(_, _, _, G),
        findall(C,
            (   retract(quad(S, P, O, G)),
                C =.. [P, S, O]
            ),
            D
        ),
        D \= [],
        conjoin(D, E),
        (   contains(G, E)
        ->  throw(term_cannot_contain_itself(G, E))
        ;   true
        ),
        assertz(graph(G, E)),
        fail
    ;   true
    ),

    % move rdf lists and values
    (   graph(A, B),
        conj_list(B, C),
        move_rdf(C, D),
        conj_list(E, D),
        E \= B,
        retract(graph(A, B)),
        assertz(graph(A, E)),
        fail
    ;   true
    ),

    % create types
    (   '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(X, Y),
        ground([X, Y]),
        getterm(X, Z),
        (   Z = X
        ->  true
        ;   retract('<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(X, Y)),
            assertz('<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>'(Z, Y))
        ),
        fail
    ;   true
    ),

    % create terms
    (   pred(P),
        P \= '<http://www.w3.org/1999/02/22-rdf-syntax-ns#first>',
        P \= '<http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies>',
        P \= '<http://www.w3.org/1999/02/22-rdf-syntax-ns#rest>',
        P \= '<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>',
        P \= '<http://www.w3.org/1999/02/22-rdf-syntax-ns#value>',
        P \= quad,
        X =.. [P, _, _],
        call(X),
        ground(X),
        getterm(X, Y),
        (   Y = X
        ->  true
        ;   retract(X),
            assertz(Y)
        ),
        fail
    ;   true
    ),

    % remove rdf lists
    retractall('<http://www.w3.org/1999/02/22-rdf-syntax-ns#first>'(_, _)),
    retractall('<http://www.w3.org/1999/02/22-rdf-syntax-ns#rest>'(_, _)),

    % remove rdf values
    retractall('<http://www.w3.org/1999/02/22-rdf-syntax-ns#value>'(_, _)),

    % remove rdf reifiers
    retractall('<http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies>'(_, _)),

    % remove rdf named graphs
    retractall(graph(_, _)),

    % create forward rules
    assertz(implies((
            '<http://www.w3.org/2000/10/swap/log#implies>'(A, B),
            ground([A, B]),
            conj_list(B, L),
            \+last(L, answer('<http://www.w3.org/2000/10/swap/log#explanation>', _, _)),
            findvars([A, B], V, alpha),
            list_to_set(V, U),
            makevars([A, B, U], [Q, I, X], beta(U)),
            (   nb_getval(explain, true),
                Q \= true,
                I \= false
            ->  conj_append(I, answer('<http://www.w3.org/2000/10/swap/log#explanation>', Q, I), F)
            ;   F = I
            )), '<http://www.w3.org/2000/10/swap/log#implies>'(Q, F))),

    % create backward rules
    assertz(implies((
            '<http://www.w3.org/2000/10/swap/log#isImpliedBy>'(B, A),
            findvars([A, B], V, alpha),
            list_to_set(V, U),
            makevars([A, B, U], [Q, I, X], beta(U)),
            (   nb_getval(explain, true),
                Q \= true,
                Q \= !
            ->  conj_append(Q, remember(answer('<http://www.w3.org/2000/10/swap/log#explanation>', Q, I)), F)
            ;   F = Q
            ),
            C = ':-'(I, F),
            copy_term_nat(C, CC),
            labelvars(CC, 0, _, avar),
            (   \+cc(CC)
            ->  assertz(cc(CC)),
                assertz(C),
                retractall(brake)
            ;   true
            )), true)),

    % create queries
    assertz(implies((
            '<http://www.w3.org/2000/10/swap/log#query>'(A, B),
            (   nb_getval(explain, true),
                A \= B
            ->  F = ('<http://www.w3.org/2000/10/swap/log#explanation>'(A, B), B)
            ;   F = B
            ),
            djiti_answer(answer(F), J),
            findvars([A, F], V, alpha),
            list_to_set(V, U),
            makevars([A, J, U], [Q, I, X], beta(U)),
            C = implies(Q, I),
            copy_term_nat(C, CC),
            labelvars(CC, 0, _, avar),
            (   \+cc(CC)
            ->  assertz(cc(CC)),
                assertz(C),
                retractall(brake)
            ;   true
            )), true)),

    % create universal statements
    (   pred(P),
        \+atom_concat('<http://www.w3.org/2000/10/swap/', _, P),
        X =.. [P, _, _],
        call(X),
        findvars(X, V, alpha),
        V \= [],
        list_to_set(V, U),
        makevars(X, Y, beta(U)),
        retract(X),
        assertz(Y),
        fail
    ;   true
    ),

    % set engine values
    findall(Sc,
        (   scope(Sc)
        ),
        Scope
    ),
    (   '<http://www.w3.org/2000/10/swap/log#explanation>'(_, _)
    ->  nb_setval(explain, false)
    ;   nb_setval(explain, true)
    ),
    nb_setval(scope, Scope),
    nb_setval(rn, 0),
    nb_setval(keep_ng, true),

    % run engine
    catch(eam(0), Exc3,
        (   (   Exc3 = halt(0)
            ->  true
            ;   format(user_error, '** ERROR ** eam ** ~w~n', [Exc3]),
                flush_output(user_error),
                (   Exc3 = inference_fuse(_)
                ->  nb_setval(exit_code, 2)
                ;   nb_setval(exit_code, 3)
                )
            )
        )
    ).

%
% command line options
%

opts([], []) :-
    !.
opts(['--genid', Genid|Argus], Args) :-
    !,
    retractall(flag(genid, _)),
    assertz(flag(genid, Genid)),
    opts(Argus, Args).
opts(['--output', File|Argus], Args) :-
    !,
    open(File, write, Out, [encoding(utf8)]),
    tell(Out),
    retractall(flag(output, _)),
    assertz(flag(output, Out)),
    opts(Argus, Args).
opts(['--wcache', Argument, File|Argus], Args) :-
    !,
    absolute_uri(Argument, Arg),
    retractall(wcache(Arg, _)),
    assertz(wcache(Arg, File)),
    opts(Argus, Args).
opts([Arg|_], _) :-
    memberchk(Arg, ['--help', '-h']),
    !,
    help_info(Help),
    format('~w~n', [Help]),
    throw(halt(0)).
opts([Arg|_], _) :-
    memberchk(Arg, ['--version', '-v']),
    !,
    version_info(Version),
    format('~w~n', [Version]),
    throw(halt(0)).
opts([Arg|_], _) :-
    \+memberchk(Arg, ['--help', '-h']),
    sub_atom(Arg, 0, 2, _, '--'),
    !,
    throw(not_supported_option(Arg)).
opts([Arg|Argus], [Arg|Args]) :-
    opts(Argus, Args).

args([]) :-
    !.
args([Argument|Args]) :-
    !,
    absolute_uri(Argument, Arg),
    atomic_list_concat(['<', Arg, '>'], R),
    assertz(scope(R)),
    cnt(doc_nr),
    (   wcacher(Arg, File)
    ->  open(File, read, In, [encoding(utf8)])
    ;   (   (   sub_atom(Arg, 0, 5, _, 'http:')
            ->  true
            ;   sub_atom(Arg, 0, 6, _, 'https:')
            )
        ->  http_open(Arg, In, []),
            set_stream(In, encoding(utf8))
        ;   (   sub_atom(Arg, 0, 5, _, 'file:')
            ->  (   parse_url(Arg, Parts)
                ->  memberchk(path(File), Parts)
                ;   sub_atom(Arg, 7, _, 0, File)
                )
            ;   File = Arg
            ),
            (   File = '-'
            ->  In = user_input
            ;   open(File, read, In, [encoding(utf8)])
            )
        )
    ),
    retractall(base_uri(_)),
    (   Arg = '-'
    ->  absolute_uri('', Abu),
        assertz(base_uri(Abu))
    ;   assertz(base_uri(Arg))
    ),
    rdf_read_turtle(stream(In), Triples, [base_uri(Arg), format(trig), prefixes(Pfxs), on_error(error)]),
    close(In),
    forall(
        member(Pfx-Ns, Pfxs),
        put_pfx(Pfx, Ns)
    ),
    (   Arg = '-'
    ->  D = '#'
    ;   atomic_list_concat([Arg, '#'], D)
    ),
    put_pfx('', D),
    forall(
        member(rdf(S, P, O), Triples),
        (   trig_term(S, Subject),
            trig_term(P, Predicate),
            trig_term(O, Object),
            Triple =.. [Predicate, Subject, Object],
            djiti_assertz(Triple),
            (   current_predicate(Predicate/2)
            ->  true
            ;   dynamic(Predicate/2)
            )
        )
    ),
    forall(
        member(rdf(S, P, O, G), Triples),
        (   trig_term(S, Subject),
            trig_term(P, Predicate),
            trig_term(O, Object),
            G = H:_,
            trig_term(H, Graph),
            assertz(quad(Subject, Predicate, Object, Graph)),
            (   current_predicate(Predicate/2)
            ->  true
            ;   dynamic(Predicate/2)
            )
        )
    ),
    args(Args).

trig_term(literal(type(A, B)), C) :-
    memberchk(A, ['http://www.w3.org/2001/XMLSchema#integer', 'http://www.w3.org/2001/XMLSchema#long', 'http://www.w3.org/2001/XMLSchema#decimal', 'http://www.w3.org/2001/XMLSchema#double']),
    atom_number(B, C),
    !.
trig_term(literal(type('http://www.w3.org/2001/XMLSchema#boolean', A)), A) :-
    !.
trig_term(literal(type(A, B)), literal(E, type(F))) :-
    atom_codes(B, C),
    escape_string(C, D),
    atom_codes(E, D),
    atomic_list_concat(['<', A, '>'], F),
    !.
trig_term(literal(lang(A, B)), literal(E, lang(A))) :-
    atom_codes(B, C),
    escape_string(C, D),
    atom_codes(E, D),
    !.
trig_term(literal(A), literal(E, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
    atom_codes(A, C),
    escape_string(C, D),
    atom_codes(E, D),
    !.
trig_term(node(A), B) :-
    !,
    nb_getval(var_ns, Sns),
    nb_getval(doc_nr, Dnr),
    atomic_list_concat(['<', Sns, 'node_', A, '_', Dnr, '>'], B).
trig_term(A, B) :-
    atomic_list_concat(['<', A, '>'], B).


% ----------------------------
% EAM (Euler Abstract Machine)
% ----------------------------

eam(Recursion) :-
    (   implies(Prem, Conc),
        ignore(Prem = true),
        catch(call_residue_vars(ucall(Prem), []), Exc,
            (   Exc = error(existence_error(procedure, _), _)
            ->  fail
            ;   throw(Exc)
            )
        ),
        (   (   Conc = false
            ;   Conc = answer(false, void, void)
            )
        ->  with_output_to(atom(Fuse), writeq('<http://www.w3.org/2000/10/swap/log#implies>'(Prem, false))),
            throw(inference_fuse(Fuse))
        ;   true
        ),
        \+atom(Conc),
        \+is_list(Conc),
        djiti_conc(Conc, Concd),
        (   Concd = ':-'(Head, Body)
        ->  \+clause(Head, Body)
        ;   (   Concd = '<http://www.w3.org/2000/10/swap/log#implies>'(_, _)
            ->  copy_term_nat(Concd, Concc),
                labelvars(Concc, 0, _, avar),
                \+cc(Concc),
                assertz(cc(Concc))
            ;   \+catch(ucall(Concd), _, fail)
            )
        ),
        (   Concd \= ':-'(_, _)
        ->  nb_getval(wn, W),
            labelvars(Prem-Concd, W, N),        % failing when Prem contains attributed variables
            nb_setval(wn, N)
        ;   true
        ),
        astep(Prem, Concd),
        retract(brake),
        fail
    ;   brake,
        (   R is Recursion+1,
            (   \+recursion(R)
            ->  assertz(recursion(R))
            ;   true
            ),
            nb_getval(limit, Limit),
            Recursion < Limit,
            eam(R)
        ;   open_null_stream(Ws),
            tell(Ws),
            nb_getval(wn, Wn),
            w3,
            forall(
                retract(keep_ng(NG)),
                (   nl,
                    wt(NG),
                    nl
                )
            ),
            forall(
                retract(keep_ng(NG)),
                (   nl,
                    wt(NG),
                    nl
                )
            ),
            retractall(pfx(_, _)),
            retractall(wpfx(_)),
            nb_setval(wn, Wn),
            forall(
                apfx(Pfx, Uri),
                assertz(pfx(Pfx, Uri))
            ),
            told,
            (   flag(output, Output)
            ->  tell(Output)
            ;   true
            ),
            w3,
            forall(
                retract(keep_ng(NG)),
                (   nl,
                    wt(NG),
                    nl
                )
            ),
            forall(
                retract(keep_ng(NG)),
                (   nl,
                    wt(NG),
                    nl
                )
            )
        ;   true
        ),
        !
    ;   assertz(brake),
        exogen,
        eam(Recursion)
    ).

astep(B, Cn) :-
    (   Cn = (Dn, En)
    ->  functor(Dn, P, N),
        (   \+pred(P),
            P \= '<http://www.w3.org/2000/10/swap/log#implies>',
            P \= '<http://www.w3.org/2000/10/swap/log#callWithCleanup>',
            N = 2
        ->  assertz(pred(P))
        ;   true
        ),
        (   Dn \= '<http://www.w3.org/2000/10/swap/log#implies>'(_, _),
            catch(call(Dn), _, fail)
        ->  true
        ;   djiti_assertz(Dn)
        ),
        astep(B, En)
    ;   (   Cn = true
        ->  true
        ;   functor(Cn, P, N),
            (   \+pred(P),
                P \= '<http://www.w3.org/2000/10/swap/log#implies>',
                P \= '<http://www.w3.org/2000/10/swap/log#callWithCleanup>',
                N = 2
            ->  assertz(pred(P))
            ;   true
            ),
            (   Cn \= '<http://www.w3.org/2000/10/swap/log#implies>'(_, _),
                catch(call(Cn), _, fail)
            ->  true
            ;   djiti_assertz(Cn)
            )
        )
    ).

%
% DJITI (Deep Just In Time Indexing)
%

djiti_answer(answer((A, B)), (C, D)) :-
    !,
    djiti_answer(answer(A), C),
    djiti_answer(answer(B), D).
djiti_answer(answer(A), answer(P, S, O)) :-
    (   nonvar(A)
    ;   atom(P),
        S \= void
    ),
    A =.. [P, S, O],
    !.
djiti_answer(answer(exopred(P, S, O)), answer(P, S, O)) :-
    (   var(S)
    ;   S \= void
    ),
    !.
djiti_answer(answer(A), answer(A, void, void)) :-
    !.
djiti_answer(A, A).

djiti_conc(':-'(exopred(P, S, O), B), ':-'(A, B)) :-
    !,
    A =.. [P, S, O].
djiti_conc(answer((A, B), void, void), (answer(A, void, void), D)) :-
    !,
    djiti_conc(answer(B, void, void), D).
djiti_conc(A, A).

djiti_fact(answer(P, S, O), answer(P, S, O)) :-
    atomic(P),
    !,
    (   P \= '<http://www.w3.org/2000/10/swap/log#callWithCleanup>',
        \+pred(P)
    ->  assertz(pred(P))
    ;   true
    ).
djiti_fact(implies(A, B), implies(A, B)) :-
    nonvar(B),
    conj_list(B, D),
    forall(
        member(E, D),
        (   unify(E, F),
            F =.. [P, _, _],
            (   \+fpred(P)
            ->  assertz(fpred(P))
            ;   true
            )
        )
    ),
    !.
djiti_fact('<http://www.w3.org/2000/10/swap/log#implies>'(A, B), C) :-
    nonvar(B),
    (   \+atomic(A)
    ;   \+atomic(B)
    ),
    (   conj_list(B, D)
    ->  true
    ;   D = B
    ),
    forall(
        member(E, D),
        (   unify(E, F),
            (   F =.. [P, _, _],
                \+fpred(P)
            ->  assertz(fpred(P))
            ;   true
            )
        )
    ),
    !,
    makevars(implies(A, B), C, zeta).
djiti_fact(':-'(A, B), ':-'(C, D)) :-
    !,
    makevars((A, B), (C, D), eta).
djiti_fact('<http://www.w3.org/2000/10/swap/log#dcg>'(_, literal(A, type('<http://www.w3.org/2001/XMLSchema#string>'))), B) :-
    !,
    read_term_from_atom(A, C, []),
    dcg_translate_rule(C, B).
djiti_fact(A, A) :-
    ground(A),
    A =.. [P, _, _],
    (   P \= '<http://www.w3.org/2000/10/swap/log#callWithCleanup>',
        P \= '<http://www.w3.org/1999/02/22-rdf-syntax-ns#first>',
        P \= '<http://www.w3.org/1999/02/22-rdf-syntax-ns#rest>',
        P \= pfx,
        P \= flag,
        \+pred(P)
    ->  assertz(pred(P))
    ;   true
    ),
    !.
djiti_fact(A, A).

djiti_assertz(A) :-
    djiti_fact(A, B),
    assertz(B).

%
% Reasoning output
%

w3 :-
    nb_setval(wpfx, false),
    forall(
        (   pfx(A, B),
            \+wpfx(A)
        ),
        (   format('PREFIX ~w ~w~n', [A, B]),
            assertz(wpfx(A)),
            nb_setval(wpfx, true)
        )
    ),
    (   nb_getval(wpfx, true)
    ->  nl
    ;   true
    ),
    nb_setval(fdepth, 0),
    nb_setval(pdepth, 0),
    nb_setval(cdepth, 0),
    (   answer(B1, B2, B3),
        B1 \= '<http://www.w3.org/2000/10/swap/log#explanation>',
        relabel([B1, B2, B3], [C1, C2, C3]),
        djiti_answer(answer(C), answer(C1, C2, C3)),
        indent,
        labelvars(C, 0, _, avar),
        wt(C),
        ws(C),
        (   (   C = graph(_, _)
            ;   C = exopred(graph, _, _)
            )
        ->  true
        ;   write('.')
        ),
        nl,
        fail
    ;   true
    ),
    (   answer('<http://www.w3.org/2000/10/swap/log#explanation>', _, _)
    ->  nl,
        writeln('#'),
        writeln('# lingua explanation'),
        writeln('#'),
        nl,
        (   answer('<http://www.w3.org/2000/10/swap/log#explanation>', S, O),
            labelvars('<http://www.w3.org/2000/10/swap/log#explanation>'(S, O), 0, _, avar),
            indent,
            wt('<http://www.w3.org/2000/10/swap/log#explanation>'(S, O)),
            ws('<http://www.w3.org/2000/10/swap/log#explanation>'(S, O)),
            write('.'),
            nl,
            fail
        ;   true
        )
    ;   true
    ).

wt(X) :-
    functor(X, _, A),
    (   A = 0,
        !,
        wt0(X)
    ;   A = 1,
        !,
        wt1(X)
    ;   A = 2,
        !,
        wt2(X)
    ;   wtn(X)
    ).

wt0(!) :-
    !,
    wq(true),
    write(' '),
    wp('<http://www.w3.org/2000/10/swap/log#callWithCut>'),
    write(' true').
wt0(:-) :-
    !,
    wp('<http://www.w3.org/2000/10/swap/log#isImpliedBy>').
wt0([]) :-
    !,
    write('()').
wt0(X) :-
    number(X),
    !,
    write(X).
wt0(X) :-
    atom(X),
    atom_concat(some, Y, X),
    !,
    (   rule_uvar(L),
        (   ncllit
        ->  (   memberchk(X, L)
            ->  true
            ;   retract(rule_uvar(L)),
                assertz(rule_uvar([X|L]))
            )
        ;   memberchk(X, L)
        )
    ->  write('var:U_')
    ;   write('_:sk_')
    ),
    write(Y).
wt0(X) :-
    atom(X),
    atom_concat(allv, Y, X),
    !,
    write('var:U_'),
    write(Y).
wt0(X) :-
    atom(X),
    atom_concat(avar, Y, X),
    !,
    atomic_list_concat(['<http://www.w3.org/2000/10/swap/var#x_', Y, '>'], Z),
    wt0(Z).
wt0(X) :-
    \+keep_skolem(X),
    nb_getval(var_ns, Sns),
    atom(X),
    sub_atom(X, 1, I, _, Sns),
    J is I+1,
    sub_atom(X, J, _, 1, Y),
    (   getlist(X, M)
    ->  wt(M)
    ;   (   rule_uvar(L),
            (   ncllit
            ->  (   memberchk(Y, L)
                ->  true
                ;   retract(rule_uvar(L)),
                    assertz(rule_uvar([Y|L]))
                )
            ;   memberchk(Y, L)
            )
        ->  (   (   sub_atom(Y, 0, 2, _, 'e_')
                ;   sub_atom(Y, 0, 3, _, 'bn_')
                )
            ->  write('_:')
            ;   sub_atom(Y, 0, 2, _, Z),
                memberchk(Z, ['x_', 't_']),
                write('var:')
            )
        ;   write('_:')
        ),
        write(Y),
        (   sub_atom(Y, 0, 2, _, 'x_')
        ->  write('_'),
            nb_getval(rn, N),
            write(N)
        ;   true
        )
    ),
    !.
wt0(X) :-
    (   wtcache(X, W)
    ->  true
    ;   (   atom(X),
            atom_codes(X, Y),
            intersection(Y, [0'!, 0'$, 0'&, 0'', 0'(, 0'), 0'*, 0'+, 0',, 0';, 0'=], []),
            (   sub_atom(X, I, 1, J, '#')
            ->  J > 1,
                sub_atom(X, 0, I, _, C),
                atom_concat(C, '#>', D)
            ;   (   sub_atom_last(X, I, 1, J, '/')
                ->  J > 1,
                    sub_atom(X, 0, I, _, C),
                    atom_concat(C, '/>', D)
                ;   (   sub_atom_last(X, I, 1, J, ':')
                    ->  sub_atom(X, 0, I, _, C),
                        atom_concat(C, ':>', D)
                    ;   J = 1,
                        D = X
                    )
                )
            ),
            pfx(E, D),
            (   apfx(E, D)
            ->  true
            ;   assertz(apfx(E, D))
            ),
            K is J-1,
            sub_atom(X, _, K, 1, F),
            \+sub_atom(F, _, 1, _, '/'),
            \+sub_atom(F, _, 1, 0, '.')
        ->  atom_concat(E, F, W),
            assertz(wtcache(X, W))
        ;   (   atom(X),
                \+ (sub_atom(X, 0, 1, _, '<'), sub_atom(X, _, 1, 0, '>')),
                \+sub_atom(X, 0, 2, _, '_:'),
                X \= true,
                X \= false
            ->  W = literal(X, type('<http://www.w3.org/2001/XMLSchema#string>'))
            ;   W = X
            )
        )
    ),
    (   W = literal(X, type('<http://www.w3.org/2001/XMLSchema#string>'))
    ->  wt2(W)
    ;   (   current_prolog_flag(windows, true)
        ->  atom_codes(W, U),
            escape_unicode(U, V),
            atom_codes(Z, V)
        ;   Z = W
        ),
        write(Z)
    ).

wt1(X) :-
    X =.. [B|C],
    (   atom(B),
        \+ (sub_atom(B, 0, 1, _, '<'), sub_atom(B, _, 1, 0, '>'))
    ->  write('"'),
        writeq(X),
        write('"')
    ;   wt(C),
        write(' '),
        wp(B),
        write(' true')
    ).

wt2((X, Y)) :-
    !,
    (   atomic(X),
        X \= '!',
        X \= true
    ->  wt2([X, Y]),
        write(' '),
        wt0('<http://www.w3.org/2000/10/swap/log#conjunction>'),
        write(' true')
    ;   wt(X),
        ws(X),
        write('.'),
        nl,
        indent,
        wt(Y)
    ).
wt2([X|Y]) :-
    !,
    write('('),
    wg(X),
    wl(Y),
    write(')').
wt2(literal(X, lang(Y))) :-
    !,
    write('"'),
    (   current_prolog_flag(windows, true)
    ->  atom_codes(X, U),
        escape_unicode(U, V),
        atom_codes(Z, V)
    ;   Z = X
    ),
    write(Z),
    write('"@'),
    write(Y).
wt2(literal(X, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
    !,
    write('"'),
    (   current_prolog_flag(windows, true)
    ->  atom_codes(X, U),
        escape_unicode(U, V),
        atom_codes(Z, V)
    ;   Z = X
    ),
    write(Z),
    write('"').
wt2(literal(X, type(Y))) :-
    !,
    write('"'),
    (   current_prolog_flag(windows, true)
    ->  atom_codes(X, U),
        escape_unicode(U, V),
        atom_codes(Z, V)
    ;   Z = X
    ),
    write(Z),
    write('"^^'),
    wt(Y).
wt2(rdiv(X, Y)) :-
    number_codes(Y, [0'1|Z]),
    lzero(Z, Z),
    !,
    (   Z = []
    ->  F = '~d.0'
    ;   length(Z, N),
        number_codes(X, U),
        (   length(U, N)
        ->  F = '0.~d'
        ;   atomic_list_concat(['~', N, 'd'], F)
        )
    ),
    format(F, [X]).
wt2(rdiv(X, Y)) :-
    !,
    format('~g', [rdiv(X, Y)]).
wt2('<http://www.w3.org/2000/10/swap/log#implies>'(X, Y)) :-
    Z = X,
    (   rule_uvar(R)
    ->  true
    ;   R = [],
        cnt(rn)
    ),
    (   nb_getval(pdepth, 0),
        nb_getval(cdepth, 0)
    ->  assertz(rule_uvar(R))
    ;   true
    ),
    (   catch(clause(Y, Z), _, fail)
    ->  (   nb_getval(fdepth, 0)
        ->  assertz(ncllit)
        ;   true
        ),
        wg(Y),
        write(' <= '),
        wg(X),
        (   nb_getval(fdepth, 0)
        ->  retract(ncllit)
        ;   true
        )
    ;   (   clause('<http://www.w3.org/2000/10/swap/log#implies>'(X, Y, _, _, _, _), true)
        ->  wg(X),
            write(' => '),
            wg(Y)
        ;   (   nb_getval(fdepth, 0)
            ->  assertz(ncllit)
            ;   true
            ),
            (   \+atom(X)
            ->  nb_getval(pdepth, PD),
                PD1 is PD+1,
                nb_setval(pdepth, PD1)
            ;   true
            ),
            wg(X),
            (   \+atom(X)
            ->  nb_setval(pdepth, PD)
            ;   true
            ),
            (   nb_getval(fdepth, 0)
            ->  retract(ncllit)
            ;   true
            ),
            write(' => '),
            (   \+atom(Y)
            ->  nb_getval(cdepth, CD),
                CD1 is CD+1,
                nb_setval(cdepth, CD1)
            ;   true
            ),
            wg(Y),
            (   \+atom(Y)
            ->  nb_setval(cdepth, CD)
            ;   true
            )
        )
    ),
    (   nb_getval(pdepth, 0),
        nb_getval(cdepth, 0)
    ->  retract(rule_uvar(_))
    ;   true
    ),
    !.
wt2(':-'(X, Y)) :-
    (   rule_uvar(R)
    ->  true
    ;   R = [],
        cnt(rn)
    ),
    (   nb_getval(fdepth, 0)
    ->  assertz(ncllit)
    ;   true
    ),
    assertz(rule_uvar(R)),
    wg(X),
    write(' <= '),
    wg(Y),
    retract(rule_uvar(U)),
    (   U \= [],
        retract(rule_uvar(V)),
        append(U, V, W)
    ->  assertz(rule_uvar(W))
    ;   true
    ),
    (   nb_getval(fdepth, 0)
    ->  retract(ncllit)
    ;   true
    ),
    !.
wt2(graph(X, Y)) :-
    !,
    write('GRAPH '),
    wp(X),
    write(' '),
    nb_setval(keep_ng, false),
    retractall(keep_ng(graph(X, Y))),
    wg(Y).
wt2(X) :-
    X =.. [P, S, O],
    (   atom(P),
        \+ (sub_atom(P, 0, 1, _, '<'), sub_atom(P, _, 1, 0, '>')),
        \+sub_atom(P, 0, 4, _, avar),
        \+sub_atom(P, 0, 4, _, allv),
        \+sub_atom(P, 0, 4, _, some),
        \+sub_atom(P, 0, 2, _, '_:'),
        P \= true,
        P \= false
    ->  write('"'),
        writeq(X),
        write('"')
    ;   wq(S),
        write(' '),
        wp(P),
        write(' '),
        wg(O)
    ).

wtn(exopred(P, S, O)) :-
    !,
    (   atom(P)
    ->  X =.. [P, S, O],
        wt2(X)
    ;   wq(S),
        write(' '),
        wg(P),
        write(' '),
        wg(O)
    ).
wtn(X) :-
    X =.. [B|C],
    (   atom(B),
        \+ (sub_atom(B, 0, 1, _, '<'), sub_atom(B, _, 1, 0, '>'))
    ->  write('"'),
        writeq(X),
        write('"')
    ;   wt(C),
        write(' '),
        wp(B),
        write(' true')
    ).

wg(X) :-
    functor(X, F, A),
    (   (   F = exopred,
            !
        ;   F = ',',
            F \= true,
            F \= false,
            F \= '-',
            F \= /,
            !
        ;   A = 2,
            F \= '.',
            F \= '[|]',
            F \= ':',
            F \= literal,
            F \= rdiv,
            (   sub_atom(F, 0, 1, _, '<'),
                sub_atom(F, _, 1, 0, '>')
            ;   sub_atom(F, 0, 2, _, '_:')
            ;   F = ':-'
            )
        )
    ->  (   nb_getval(keep_ng, true)
        ->  (   graph(N, X)
            ->  true
            ;   gensym('bng_', Y),
                nb_getval(var_ns, Sns),
                atomic_list_concat(['<', Sns, Y, '>'], N),
                assertz(graph(N, X))
            ),
            (   \+keep_ng(graph(N, X))
            ->  assertz(keep_ng(graph(N, X)))
            ;   true
            ),
            wt(N)
        ;   nb_setval(keep_ng, true),
            write('{'),
            indentation(4),
            nl,
            indent,
            nb_getval(fdepth, D),
            E is D+1,
            nb_setval(fdepth, E),
            wt(X),
            nb_setval(fdepth, D),
            indentation(-4),
            write('.'),
            nl,
            indent,
            write('}')
        )
    ;   wt(X)
    ).

wp('<http://www.w3.org/1999/02/22-rdf-syntax-ns#type>') :-
    !,
    write('a').
wp(X) :-
    wg(X).

wq(A) :-
    (   raw_type(A, '<http://www.w3.org/2000/10/swap/log#Literal>')
    ->  write('[] '),
        wp('<http://www.w3.org/1999/02/22-rdf-syntax-ns#value>'),
        write(' '),
        wt(A),
        write(';')
    ;   wg(A)
    ).

wl([]) :-
    !.
wl([X|Y]) :-
    write(' '),
    wg(X),
    wl(Y).

ws((X, Y)) :-
    !,
    conj_list((X, Y), Z),
    last(Z, U),
    ws(U).
ws(X) :-
    X =.. Y,
    last(Y, Z),
    (   \+number(Z),
        Z \= rdiv(_, _)
    ->  true
    ;   write(' ')
    ).

indent:-
    nb_getval(indentation, A),
    tab(A).

indentation(C) :-
    nb_getval(indentation, A),
    B is A+C,
    nb_setval(indentation, B).

%
% Built-ins
%

'<http://www.w3.org/1999/02/22-rdf-syntax-ns#first>'(X, Y) :-
    when(
        (   nonvar(X)
        ;   nonvar(Y)
        ),
        (   X = [Y|_]
        )
    ),
    !.

'<http://www.w3.org/1999/02/22-rdf-syntax-ns#rest>'(X, Y) :-
    when(
        (   nonvar(X)
        ;   nonvar(Y)
        ),
        (   X = [_|Y]
        )
    ),
    !.

'<http://www.w3.org/2000/10/swap/crypto#md5>'(literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')), literal(B, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
    when(
        (   nonvar(A)
        ),
        (   md5_hash(A, B, [])
        )
    ).

'<http://www.w3.org/2000/10/swap/crypto#sha>'(literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')), literal(B, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
    when(
        (   nonvar(A)
        ),
        (   sha_hash(A, C, [algorithm(sha1)]),
            hash_atom(C, B)
        )
    ).

'<http://www.w3.org/2000/10/swap/crypto#sha256>'(literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')), literal(B, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
    when(
        (   nonvar(A)
        ),
        (   sha_hash(A, C, [algorithm(sha256)]),
            hash_atom(C, B)
        )
    ).

'<http://www.w3.org/2000/10/swap/crypto#sha512>'(literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')), literal(B, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
    when(
        (   nonvar(A)
        ),
        (   sha_hash(A, C, [algorithm(sha512)]),
            hash_atom(C, B)
        )
    ).

'<http://www.w3.org/2000/10/swap/graph#difference>'(A, B) :-
    when(
        (   nonvar(A)
        ),
        (   makevars(A, C, delta),
            difference(C, B)
        )
    ).

'<http://www.w3.org/2000/10/swap/graph#intersection>'(A, B) :-
    when(
        (   nonvar(A)
        ),
        (   intersect(A, B)
        )
    ).

'<http://www.w3.org/2000/10/swap/graph#length>'(A, B) :-
    when(
        (   nonvar(A)
        ),
        (   conj_list(A, D),
            (   ground(D)
            ->  distinct(D, C)
            ;   C = D
            ),
            length(C, B)
        )
    ).

'<http://www.w3.org/2000/10/swap/graph#list>'(A, B) :-
    conj_list(A, B).

'<http://www.w3.org/2000/10/swap/graph#member>'(A, B) :-
    when(
        (   nonvar(A)
        ),
        (   conj_list(A, C),
            member(B, C)
        )
    ).

'<http://www.w3.org/2000/10/swap/graph#notMember>'(A, B) :-
    when(
        (   nonvar(A)
        ),
        (   conj_list(A, C),
            \+member(B, C)
        )
    ).

'<http://www.w3.org/2000/10/swap/graph#renameBlanks>'(A, B) :-
    when(
        (   nonvar(A)
        ),
        (   copy_term_nat(A, C),
            findvars(C, D, beta),
            makevars(C, B, beta(D)),
            nb_getval(wn, W),
            labelvars(B, W, N),
            nb_setval(wn, N)
        )
    ).

'<http://www.w3.org/2000/10/swap/graph#union>'(A, B) :-
    when(
        (   nonvar(A)
        ),
        (   conjoin(A, B)
        )
    ).

'<http://www.w3.org/2000/10/swap/list#append>'(A, B) :-
    when(
        (   nonvar(A)
        ),
        (   getlist(A, C),
            (   member(D, C),
                var(D),
                var(B)
            ->  true
            ;   append(C, B)
            )
        )
    ).

'<http://www.w3.org/2000/10/swap/list#first>'(A, B) :-
    when(
        (   nonvar(A)
        ),
        (   getlist(A, C),
            C = [B|D],
            nonvar(D)
        )
    ).

'<http://www.w3.org/2000/10/swap/list#firstRest>'(A, [B, C]) :-
    getlist(A, D),
    D = [B|C].

'<http://www.w3.org/2000/10/swap/list#in>'(A, B) :-
    when(
        (   nonvar(B)
        ),
        (   getlist(B, C),
            member(A, C)
        )
    ).

'<http://www.w3.org/2000/10/swap/list#isList>'(A, B) :-
    (   getlist(A, _)
    ->  B = true
    ;   B = false
    ).

'<http://www.w3.org/2000/10/swap/list#iterate>'(A, [B, C]) :-
    when(
        (   nonvar(A)
        ),
        (   getlist(A, D),
            nth0(B, D, C)
        )
    ).

'<http://www.w3.org/2000/10/swap/list#last>'(A, B) :-
    when(
        (   nonvar(A)
        ),
        (   getlist(A, C),
            last(C, B)
        )
    ).

'<http://www.w3.org/2000/10/swap/list#length>'(A, B) :-
    when(
        (   nonvar(A)
        ),
        (   (   getlist(A, C)
            ->  true
            ;   conj_list(A, D),
                (   ground(D)
                ->  distinct(D, C)
                ;   C = D
                )
            ),
            length(C, B)
        )
    ).

'<http://www.w3.org/2000/10/swap/list#map>'([A, B], C) :-
    when(
        (   nonvar(A),
            nonvar(B)
        ),
        (   getlist(A, D),
            findall(E,
                (   member(F, D),
                    G =.. [B, F, E],
                    G
                ),
                C
            )
        )
    ).

'<http://www.w3.org/2000/10/swap/list#member>'(A, B) :-
    when(
        (   nonvar(A)
        ),
        (   getlist(A, C),
            member(B, C)
        )
    ).

'<http://www.w3.org/2000/10/swap/list#memberAt>'([A, B], C) :-
    when(
        (   nonvar(A)
        ),
        (   nth0(B, A, C)
        )
    ).

'<http://www.w3.org/2000/10/swap/list#multisetEqualTo>'(A, B) :-
    when(
        (   nonvar(A),
            nonvar(B)
        ),
        (   sort(0, @=<, A, C),
            sort(0, @=<, B, C)
        )
    ).

'<http://www.w3.org/2000/10/swap/list#multisetNotEqualTo>'(A, B) :-
    \+'<http://www.w3.org/2000/10/swap/list#multisetEqualTo>'(A, B).

'<http://www.w3.org/2000/10/swap/list#notMember>'(A, B) :-
    when(
        (   nonvar(A)
        ),
        (   getlist(A, C),
            \+member(B, C)
        )
    ).

'<http://www.w3.org/2000/10/swap/list#remove>'([A, B], C) :-
    when(
        (   nonvar(A),
            nonvar(B)
        ),
        (   selectchk(B, A, D),
            (   \+member(B, D)
            ->  C = D
            ;   '<http://www.w3.org/2000/10/swap/list#remove>'([D, B], C)
            )
        )
    ).

'<http://www.w3.org/2000/10/swap/list#removeAt>'([A, B], C) :-
    when(
        (   nonvar(A)
        ),
        (   nth0(B, A, D),
            selectchk(D, A, C)
        )
    ).

'<http://www.w3.org/2000/10/swap/list#removeDuplicates>'(A, B) :-
    when(
        (   nonvar(A)
        ),
        (   getlist(A, C),
            list_to_set(C, B)
        )
    ).

'<http://www.w3.org/2000/10/swap/list#rest>'(A, B) :-
    when(
        (   nonvar(A)
        ),
        (   getlist(A, C),
            C = [_|B]
        )
    ).

'<http://www.w3.org/2000/10/swap/list#select>'(A, [B, C]) :-
    when(
        (   nonvar(A)
        ),
        (   getlist(A, D),
            select(B, D, C)
        )
    ).

'<http://www.w3.org/2000/10/swap/list#setEqualTo>'(A, B) :-
    when(
        (   nonvar(A),
            nonvar(B)
        ),
        (   getlist(A, C),
            getlist(B, D),
            sort(C, E),
            sort(D, E)
        )
    ).

'<http://www.w3.org/2000/10/swap/list#setNotEqualTo>'(A, B) :-
    \+'<http://www.w3.org/2000/10/swap/list#setEqualTo>'(A, B).

'<http://www.w3.org/2000/10/swap/list#sort>'(A, B) :-
    when(
        (   nonvar(A)
        ),
        (   sort(A, B)
        )
    ).

'<http://www.w3.org/2000/10/swap/list#unique>'(A, B) :-
    when(
        (   nonvar(A)
        ),
        (   list_to_set(A, B)
        )
    ).

'<http://www.w3.org/2000/10/swap/log#becomes>'(A, B) :-
    catch(call(A), _, fail),
    A \= B,
    unify(A, C),
    conj_list(C, D),
    forall(
        member(E, D),
        (   (   E = '<http://www.w3.org/2000/10/swap/log#implies>'(Prem, Conc)
            ->  retract(implies(Prem, Conc))
            ;   (   E = ':-'(Ci, Pi),
                    Pi \= true
                ->  retract(':-'(Ci, Pi))
                ;   E \= ':-'(_, true),
                    retract(E)
                )
            ),
            djiti_answer(answer(E), Z),
            retractall(Z)
        )
    ),
    nb_getval(wn, W),
    labelvars(B, W, N),
    nb_setval(wn, N),
    unify(B, F),
    conj_list(F, G),
    forall(
        member(H, G),
        djiti_assertz(H)
    ).

'<http://www.w3.org/2000/10/swap/log#bound>'(X, Y) :-
    (   nonvar(X)
    ->  Y = true
    ;   Y = false
    ).

'<http://www.w3.org/2000/10/swap/log#call>'(A, B) :-
    call(A),
    catch(call(B), _, fail).

'<http://www.w3.org/2000/10/swap/log#callNotBind>'(A, B) :-
    \+ \+call(A),
    \+ \+catch(call(B), _, fail).

'<http://www.w3.org/2000/10/swap/log#callWithCleanup>'(A, B) :-
    call_cleanup(A, B).

'<http://www.w3.org/2000/10/swap/log#callWithOptional>'(A, B) :-
    call(A),
    (   \+catch(call(B), _, fail)
    ->  true
    ;   catch(call(B), _, fail)
    ).

'<http://www.w3.org/2000/10/swap/log#collectAllIn>'([A, B, C], Sc) :-
    within_scope(Sc),
    nonvar(B),
    \+is_list(B),
    catch(findall(A, B, E), _, E = []),
    E = C.

'<http://www.w3.org/2000/10/swap/log#conjunction>'(A, B) :-
    when(
        (   nonvar(A)
        ),
        (   conjoin(A, M),
            unify(M, B)
        )
    ).

'<http://www.w3.org/2000/10/swap/log#copy>'(X, Y) :-
    copy_term_nat(X, Y).

'<http://www.w3.org/2000/10/swap/log#dtlit>'([A, B], C) :-
    when(
        (   ground(A)
        ;   nonvar(C)
        ),
        (   ground(A),
            (   var(B)
            ->  (   member(B, ['<http://www.w3.org/2001/XMLSchema#integer>', '<http://www.w3.org/2001/XMLSchema#double>',
                    '<http://www.w3.org/2001/XMLSchema#date>', '<http://www.w3.org/2001/XMLSchema#time>', '<http://www.w3.org/2001/XMLSchema#dateTime>',
                    '<http://www.w3.org/2001/XMLSchema#yearMonthDuration>', '<http://www.w3.org/2001/XMLSchema#dayTimeDuration>', '<http://www.w3.org/2001/XMLSchema#duration>']),
                    dtlit([A, B], C),
                    getnumber(C, D),
                    dtlit([_, B], D)
                ->  true
                ;   (   dtlit([A, '<http://www.w3.org/2001/XMLSchema#boolean>'], C),
                        getbool(C, _),
                        B = '<http://www.w3.org/2001/XMLSchema#boolean>'
                    ->  true
                    ;   B = '<http://www.w3.org/2001/XMLSchema#string>',
                        C = A
                    )
                )
            ;   A = literal(E, _),
                (   B = prolog:atom
                ->  C = E
                ;   C = literal(E, type(B))
                ),
                !
            )
        ;   nonvar(C),
            dtlit([A, B], C)
        )
    ).

'<http://www.w3.org/2000/10/swap/log#equalTo>'(X, Y) :-
    unify(X, Y).

'<http://www.w3.org/2000/10/swap/log#forAllIn>'([A, B], Sc) :-
    within_scope(Sc),
    when(
        (   nonvar(A),
            nonvar(B)
        ),
        (   forall(A, B)
        )
    ).

'<http://www.w3.org/2000/10/swap/log#hasPrefix>'(A, B) :-
    when(
        (   nonvar(A)
        ),
        (   pfx(_, A)
        ->  B = true
        ;   B = false
        )
    ).

'<http://www.w3.org/2000/10/swap/log#ifThenElseIn>'([A, B, C], Sc) :-
    within_scope(Sc),
    when(
        (   nonvar(A),
            nonvar(B),
            nonvar(C)
        ),
        (   if_then_else(A, B, C)
        )
    ).

'<http://www.w3.org/2000/10/swap/log#implies>'(A, B) :-
    implies(U, V),
    unify(U, A),
    unify(V, B),
    (   commonvars(A, B, [])
    ->  labelvars(B, 0, _, avar)
    ;   true
    ),
    (   var(B)
    ->  true
    ;   B \= answer(_, _, _),
        B \= (answer(_, _, _), _)
    ).

'<http://www.w3.org/2000/10/swap/log#imports>'(_, X) :-
    when(
        (   nonvar(X)
        ),
        (   (   scope(X)
            ->  true
            ;   sub_atom(X, 0, 1, _, '<'),
                sub_atom(X, _, 1, 0, '>'),
                sub_atom(X, 1, _, 1, Z),
                args([Z])
            )
        )
    ).

'<http://www.w3.org/2000/10/swap/log#includes>'(X, Y) :-
    within_scope(X),
    !,
    when(
        (   nonvar(Y)
        ),
        (   call(Y)
        )
    ).
'<http://www.w3.org/2000/10/swap/log#includes>'(X, Y) :-
    when(
        (   nonvar(X),
            nonvar(Y)
        ),
        (   X \= [_, _],
            conj_list(X, A),
            conj_list(Y, B),
            includes(A, B)
        )
    ).

'<http://www.w3.org/2000/10/swap/log#includesNotBind>'(X, Y) :-
    within_scope(X),
    !,
    when(
        (   nonvar(Y)
        ),
        (   \+ \+call(Y)
        )
    ).
'<http://www.w3.org/2000/10/swap/log#includesNotBind>'(X, Y) :-
    when(
        (   nonvar(X),
            nonvar(Y)
        ),
        (   conj_list(X, A),
            conj_list(Y, B),
            \+ \+includes(A, B)
        )
    ).

'<http://www.w3.org/2000/10/swap/log#inferences>'(A, B) :-
    '<http://www.w3.org/2000/10/swap/log#conclusion>'(A, C),
    (   nonvar(B)
    ->  intersect([B, C], M),
        unify(M, B)
    ;   B = C
    ).

'<http://www.w3.org/2000/10/swap/log#isomorphic>'(A, B) :-
    makevars([A, B], [C, D], beta),
    \+ \+unify(C, B),
    \+ \+unify(A, D).

'<http://www.w3.org/2000/10/swap/log#langlit>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')), literal(B, type('<http://www.w3.org/2001/XMLSchema#string>'))], literal(A, lang(B))).

'<http://www.w3.org/2000/10/swap/log#localName>'(A, literal(B, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
    when(
        (   nonvar(A)
        ),
        (   sub_atom(A, 1, _, 1, C),
            (   sub_atom_last(C, _, 1, N, '#') ->
                sub_atom(C, _, N, 0, B)
                ;
                sub_atom_last(C, _, 1, N, '/') ->
                sub_atom(C, _, N, 0, B)
                ;
                sub_atom_last(C, _, 1, N, ':') ->
                sub_atom(C, _, N, 0, B)
            )
        )
    ).

'<http://www.w3.org/2000/10/swap/log#namespace>'(A, literal(B, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
    when(
        (   nonvar(A)
        ),
        (   sub_atom(A, 1, _, 1, C),
            (   sub_atom_last(C, N, 1, _, '#') ->
                M is N+1,
                sub_atom(C, 0, M, _, B)
                ;
                sub_atom_last(C, N, 1, _, '/') ->
                M is N+1,
                sub_atom(C, 0, M, _, B)
                ;
                sub_atom_last(C, N, 1, _, ':') ->
                M is N+1,
                sub_atom(C, 0, M, _, B)
            )
        )
    ).

'<http://www.w3.org/2000/10/swap/log#notEqualTo>'(X, Y) :-
    when(
        (   ground([X, Y])
        ),
        (   (   \+atomic(X),
                \+atomic(Y)
            ->  findvars([X, Y], Z, beta),
                Z = []
            ;   true
            ),
            \+'<http://www.w3.org/2000/10/swap/log#equalTo>'(X, Y)
        )
    ).

'<http://www.w3.org/2000/10/swap/log#notIncludes>'(X, Y) :-
    within_scope(X),
    !,
    when(
        (   nonvar(Y)
        ),
        (   \+call(Y)
        )
    ).
'<http://www.w3.org/2000/10/swap/log#notIncludes>'(X, Y) :-
    when(
        (   nonvar(X),
            nonvar(Y)
        ),
        (   X \= [_, _],
            conj_list(X, A),
            conj_list(Y, B),
            \+includes(A, B)
        )
    ).

'<http://www.w3.org/2000/10/swap/log#notIsomorphic>'(X, Y) :-
    \+'<http://www.w3.org/2000/10/swap/log#isomorphic>'(X, Y).

'<http://www.w3.org/2000/10/swap/log#phrase>'([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>'))|B], C) :-
    read_term_from_atom(A, D, [variables(B)]),
    findall(E,
        (   member(literal(E, type('<http://www.w3.org/2001/XMLSchema#string>')), C)
        ),
        F
    ),
    phrase(D, F, []).

'<http://www.w3.org/2000/10/swap/log#prefix>'(A, literal(B, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
    when(
        (   nonvar(A)
        ;   nonvar(B)
        ),
        (   pfx(C, A),
            sub_atom(C, 0, _, 1, B)
        )
    ).

'<http://www.w3.org/2000/10/swap/log#racine>'(A, B) :-
    when(
        (   nonvar(A)
        ),
        (   sub_atom(A, 1, _, 1, C),
            sub_atom(C, N, 1, _, '#'),
            sub_atom(C, 0, N, _, D),
            atomic_list_concat(['<', D, '>'], B)
        )
    ).

'<http://www.w3.org/2000/10/swap/log#rawType>'(A, B) :-
    raw_type(A, C),
    C = B.

'<http://www.w3.org/2000/10/swap/log#repeat>'(A, B) :-
    C is A-1,
    between(0, C, B).

'<http://www.w3.org/2000/10/swap/log#skolem>'(Y, X) :-
    when(
        (   nonvar(X)
        ;   ground(Y)
        ),
        (   (   is_list(Y),
                length(Y, I),
                I < 8
            ->  Z =.. [tuple, X|Y]
            ;   Z = tuple(X, Y)
            ),
            (   call(Z)
            ->  true
            ;   var(X),
                nb_getval(tuple, M),
                N is M+1,
                nb_setval(tuple, N),
                atom_number(A, N),
                nb_getval(var_ns, Sns),
                atomic_list_concat(['<', Sns, 't_', A, '>'], X),
                assertz(Z)
            )
        )
    ),
    (   \+keep_skolem(X)
    ->  assertz(keep_skolem(X))
    ;   true
    ).

'<http://www.w3.org/2000/10/swap/log#trace>'(X, Y) :-
    tell(user_error),
    copy_term_nat(X, U),
    wg(U),
    write(' TRACE '),
    copy_term_nat(Y, V),
    (   number(X),
        X < 0
    ->  fm(V)
    ;   wg(V)
    ),
    nl,
    told,
    (   flag(output, Output)
    ->  tell(Output)
    ;   true
    ).

'<http://www.w3.org/2000/10/swap/log#uri>'(X, Y) :-
    when(
        (   ground(X)
        ;   ground(Y)
        ),
        (   atomic(X),
            X \= [],
            (   atom_concat(some, V, X)
            ->  nb_getval(var_ns, Sns),
                atomic_list_concat(['<', Sns, 'sk_', V, '>'], U)
            ;   (   atom_concat(avar, V, X)
                ->  atomic_list_concat(['<http://www.w3.org/2000/10/swap/var#x_', V, '>'], U)
                ;   U = X
                )
            ),
            sub_atom(U, 1, _, 1, Z),
            atomic_list_concat(['<', Z, '>'], U),
            Y = literal(Z, type('<http://www.w3.org/2001/XMLSchema#string>')),
            !
        ;   ground(Y),
            Y = literal(Z, type('<http://www.w3.org/2001/XMLSchema#string>')),
            atomic_list_concat(['<', Z, '>'], X)
        )
    ).

'<http://www.w3.org/2000/10/swap/log#uuid>'(X, literal(Y, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
    when(
        (   ground(X)
        ),
        (   '<http://www.w3.org/2000/10/swap/log#uri>'(X, literal(U, type('<http://www.w3.org/2001/XMLSchema#string>'))),
            (   uuid(U, Y)
            ->  true
            ;   uuid(Y),
                assertz(uuid(U, Y))
            )
        )
    ).

'<http://www.w3.org/2000/10/swap/log#version>'(_, literal(V, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
    version_info(V).

'<http://www.w3.org/2000/10/swap/math#absoluteValue>'(X, Y) :-
    when(
        (   ground(X)
        ),
        (   getnumber(X, U),
            Y is abs(U)
        )
    ).

'<http://www.w3.org/2000/10/swap/math#acos>'(X, Y) :-
    when(
        (   ground(X)
        ;   ground(Y)
        ),
        (   getnumber(X, U),
            Y is acos(U),
            !
        ;   getnumber(Y, V),
            X is cos(V)
        )
    ).

'<http://www.w3.org/2000/10/swap/math#acosh>'(X, Y) :-
    when(
        (   ground(X)
        ;   ground(Y)
        ),
        (   getnumber(X, U),
            Y is acosh(U),
            !
        ;   getnumber(Y, V),
            X is cosh(V)
        )
    ).

'<http://www.w3.org/2000/10/swap/math#asin>'(X, Y) :-
    when(
        (   ground(X)
        ;   ground(Y)
        ),
        (   getnumber(X, U),
            Y is asin(U),
            !
        ;   getnumber(Y, V),
            X is sin(V)
        )
    ).

'<http://www.w3.org/2000/10/swap/math#asinh>'(X, Y) :-
    when(
        (   ground(X)
        ;   ground(Y)
        ),
        (   getnumber(X, U),
            Y is asinh(U),
            !
        ;   getnumber(Y, V),
            X is sinh(V)
        )
    ).

'<http://www.w3.org/2000/10/swap/math#atan>'(X, Y) :-
    when(
        (   ground(X)
        ;   ground(Y)
        ),
        (   getnumber(X, U),
            Y is atan(U),
            !
        ;   getnumber(Y, V),
            X is tan(V)
        )
    ).

'<http://www.w3.org/2000/10/swap/math#atan2>'([X, Y], Z) :-
    when(
        (   ground([X, Y])
        ),
        (   getnumber(X, U),
            getnumber(Y, V),
            Z is atan(U/V)
        )
    ).

'<http://www.w3.org/2000/10/swap/math#atanh>'(X, Y) :-
    when(
        (   ground(X)
        ;   ground(Y)
        ),
        (   getnumber(X, U),
            Y is atanh(U),
            !
        ;   getnumber(Y, V),
            X is tanh(V)
        )
    ).

'<http://www.w3.org/2000/10/swap/math#ceiling>'(X, Y) :-
    when(
        (   ground(X)
        ),
        (   getnumber(X, U),
            Y is ceiling(U)
        )
    ).

'<http://www.w3.org/2000/10/swap/math#cos>'(X, Y) :-
    when(
        (   ground(X)
        ;   ground(Y)
        ),
        (   getnumber(X, U),
            Y is cos(U),
            !
        ;   getnumber(Y, V),
            X is acos(V)
        )
    ).

'<http://www.w3.org/2000/10/swap/math#cosh>'(X, Y) :-
    when(
        (   ground(X)
        ;   ground(Y)
        ),
        (   getnumber(X, U),
            Y is cosh(U),
            !
        ;   getnumber(Y, V),
            X is acosh(V)
        )
    ).

'<http://www.w3.org/2000/10/swap/math#degrees>'(X, Y) :-
    when(
        (   ground(X)
        ;   ground(Y)
        ),
        (   getnumber(X, U),
            Y is U*180/pi,
            !
        ;   getnumber(Y, V),
            X is V*pi/180
        )
    ).

'<http://www.w3.org/2000/10/swap/math#difference>'([X, Y], Z) :-
    when(
        (   ground([X, Y])
        ;   ground(Z)
        ),
        (   getnumber(X, U),
            getnumber(Y, V),
            Z is U-V,
            !
        ;   getnumber(X, U),
            getnumber(Z, W),
            Y is U-W,
            !
        ;   getnumber(Y, V),
            getnumber(Z, W),
            X is V+W
        )
    ).

'<http://www.w3.org/2000/10/swap/math#equalTo>'(X, Y) :-
    when(
        (   ground([X, Y])
        ),
        (   getnumber(X, U),
            getnumber(Y, V),
            U =:= V
        )
    ).

'<http://www.w3.org/2000/10/swap/math#exponentiation>'([X, Y], Z) :-
    when(
        (   ground([X, Y])
        ;   ground([X, Z])
        ),
        (   getnumber(X, U),
            (   getnumber(Y, V),
                Z is U**V,
                !
            ;   getnumber(Z, W),
                W =\= 0,
                U =\= 0,
                Y is log(W)/log(U)
            )
        )
    ).

'<http://www.w3.org/2000/10/swap/math#floor>'(X, Y) :-
    when(
        (   ground(X)
        ),
        (   getnumber(X, U),
            Y is floor(U)
        )
    ).

'<http://www.w3.org/2000/10/swap/math#greaterThan>'(X, Y) :-
    when(
        (   ground([X, Y])
        ),
        (   getnumber(X, U),
            getnumber(Y, V),
            U > V
        )
    ).

'<http://www.w3.org/2000/10/swap/math#integerQuotient>'([X, Y], Z) :-
    when(
        (   ground([X, Y])
        ),
        (   getnumber(X, U),
            getnumber(Y, V),
            (   V =\= 0
            ->  Z is round(floor(U/V))
            ;   throw(zero_division('<http://www.w3.org/2000/10/swap/math#integerQuotient>'([X, Y], Z)))
            )
        )
    ).

'<http://www.w3.org/2000/10/swap/math#lessThan>'(X, Y) :-
    when(
        (   ground([X, Y])
        ),
        (   getnumber(X, U),
            getnumber(Y, V),
            U < V
        )
    ).

'<http://www.w3.org/2000/10/swap/math#logarithm>'([X, Y], Z) :-
    when(
        (   ground([X, Y])
        ;   ground([X, Z])
        ),
        (   getnumber(X, U),
            (   getnumber(Y, V),
                V =\= 0,
                U =\= 0,
                Z is log(U)/log(V),
                !
            ;   getnumber(Z, W),
                Y is U**(1/W)
            )
        )
    ).

'<http://www.w3.org/2000/10/swap/math#max>'(X, Y) :-
    when(
        (   ground(X)
        ),
        (   findall(A,
                (   member(B, X),
                    getnumber(B, A)
                ),
                Z
            ),
            max_list(Z, Y)
        )
    ).

'<http://www.w3.org/2000/10/swap/math#memberCount>'(X, Y) :-
    when(
        (   nonvar(X)
        ),
        (   (   getlist(X, Z)
            ->  true
            ;   conj_list(X, U),
                (   ground(U)
                ->  distinct(U, Z)
                ;   Z = U
                )
            ),
            length(Z, Y)
        )
    ).

'<http://www.w3.org/2000/10/swap/math#min>'(X, Y) :-
    when(
        (   ground(X)
        ),
        (   findall(A,
                (   member(B, X),
                    getnumber(B, A)
                ),
                Z
            ),
            min_list(Z, Y)
        )
    ).

'<http://www.w3.org/2000/10/swap/math#negation>'(X, Y) :-
    when(
        (   ground(X)
        ;   ground(Y)
        ),
        (   getnumber(X, U),
            Y is -U,
            !
        ;   getnumber(Y, V),
            X is -V
        )
    ).

'<http://www.w3.org/2000/10/swap/math#notEqualTo>'(X, Y) :-
    when(
        (   ground([X, Y])
        ),
        (   getnumber(X, U),
            getnumber(Y, V),
            U =\= V
        )
    ).

'<http://www.w3.org/2000/10/swap/math#notGreaterThan>'(X, Y) :-
    when(
        (   ground([X, Y])
        ),
        (   getnumber(X, U),
            getnumber(Y, V),
            U =< V
        )
    ).

'<http://www.w3.org/2000/10/swap/math#notLessThan>'(X, Y) :-
    when(
        (   ground([X, Y])
        ),
        (   getnumber(X, U),
            getnumber(Y, V),
            U >= V
        )
    ).

'<http://www.w3.org/2000/10/swap/math#product>'(X, Y) :-
    when(
        (   ground(X)
        ),
        (   product(X, Y)
        )
    ).

'<http://www.w3.org/2000/10/swap/math#quotient>'([X, Y], Z) :-
    when(
        (   ground([X, Y])
        ;   ground(Z)
        ),
        (   getnumber(X, U),
            getnumber(Y, V),
            (   V =\= 0
            ->  Z is U/V
            ;   throw(zero_division('<http://www.w3.org/2000/10/swap/math#quotient>'([X, Y], Z)))
            ),
            !
        ;   getnumber(X, U),
            getnumber(Z, W),
            (   W =\= 0
            ->  Y is U/W
            ;   throw(zero_division('<http://www.w3.org/2000/10/swap/math#quotient>'([X, Y], Z)))
            ),
            !
        ;   getnumber(Y, V),
            getnumber(Z, W),
            X is V*W
        )
    ).

'<http://www.w3.org/2000/10/swap/math#radians>'(X, Y) :-
    when(
        (   ground(X)
        ;   ground(Y)
        ),
        (   getnumber(X, U),
            Y is U*pi/180,
            !
        ;   getnumber(Y, V),
            X is V*180/pi
        )
    ).

'<http://www.w3.org/2000/10/swap/math#remainder>'([X, Y], Z) :-
    when(
        (   ground([X, Y])
        ),
        (   getnumber(X, U),
            getnumber(Y, V),
            (   V =\= 0
            ->  Z is U-V*round(floor(U/V))
            ;   throw(zero_division('<http://www.w3.org/2000/10/swap/math#remainder>'([X, Y], Z)))
            )
        )
    ).

'<http://www.w3.org/2000/10/swap/math#rounded>'(X, Y) :-
    when(
        (   ground(X)
        ),
        (   getnumber(X, U),
            Y is round(round(U))
        )
    ).

'<http://www.w3.org/2000/10/swap/math#roundedTo>'([X, Y], Z) :-
    when(
        (   ground([X, Y])
        ),
        (   getnumber(X, U),
            getnumber(Y, V),
            F is 10**floor(V),
            Z is round(round(U*F))/F
        )
    ).

'<http://www.w3.org/2000/10/swap/math#sin>'(X, Y) :-
    when(
        (   ground(X)
        ;   ground(Y)
        ),
        (   getnumber(X, U),
            Y is sin(U),
            !
        ;   getnumber(Y, V),
            X is asin(V)
        )
    ).

'<http://www.w3.org/2000/10/swap/math#sinh>'(X, Y) :-
    when(
        (   ground(X)
        ;   ground(Y)
        ),
        (   getnumber(X, U),
            Y is sinh(U),
            !
        ;   getnumber(Y, V),
            X is asinh(V)
        )
    ).

'<http://www.w3.org/2000/10/swap/math#sum>'(X, Y) :-
    when(
        (   ground(X)
        ),
        (   sum(X, Y)
        )
    ).

'<http://www.w3.org/2000/10/swap/math#tan>'(X, Y) :-
    when(
        (   ground(X)
        ;   ground(Y)
        ),
        (   getnumber(X, U),
            Y is tan(U),
            !
        ;   getnumber(Y, V),
            X is atan(V)
        )
    ).

'<http://www.w3.org/2000/10/swap/math#tanh>'(X, Y) :-
    when(
        (   ground(X)
        ;   ground(Y)
        ),
        (   getnumber(X, U),
            Y is tanh(U),
            !
        ;   getnumber(Y, V),
            X is atanh(V)
        )
    ).

'<http://www.w3.org/2000/10/swap/string#capitalize>'(literal(X, Z), literal(Y, Z)) :-
    when(
        (   ground(X)
        ),
        (   sub_atom(X, 0, 1, _, A),
            upcase_atom(A, B),
            sub_atom(X, 1, _, 0, C),
            downcase_atom(C, D),
            atom_concat(B, D, Y)
        )
    ).

'<http://www.w3.org/2000/10/swap/string#concatenation>'(X, Y) :-
    when(
        (   ground(X)
        ),
        (   getlist(X, C),
            labelvars(C, 0, _, avar),
            (   member(D, C),
                var(D),
                var(Y)
            ->  true
            ;   findall(S,
                    (   member(A, X),
                        getcodes(A, S)
                    ),
                    Z
                ),
                flatten(Z, E),
                atom_codes(F, E),
                Y = literal(F, type('<http://www.w3.org/2001/XMLSchema#string>'))
            )
        )
    ).

'<http://www.w3.org/2000/10/swap/string#contains>'(literal(X, Z), literal(Y, Z)) :-
    when(
        (   ground([X, Y])
        ),
        (   sub_atom(X, _, _, _, Y)
        )
    ).

'<http://www.w3.org/2000/10/swap/string#containsIgnoringCase>'(literal(X, Z), literal(Y, Z)) :-
    when(
        (   ground([X, Y])
        ),
        (   downcase_atom(X, U),
            downcase_atom(Y, V),
            sub_atom(U, _, _, _, V)
        )
    ).

'<http://www.w3.org/2000/10/swap/string#containsRoughly>'(literal(X, Z), literal(Y, Z)) :-
    when(
        (   ground([X, Y])
        ),
        (   downcase_atom(X, R),
            downcase_atom(Y, S),
            normalize_space(atom(U), R),
            normalize_space(atom(V), S),
            sub_atom(U, _, _, _, V)
        )
    ).

'<http://www.w3.org/2000/10/swap/string#endsWith>'(literal(X, _), literal(Y, _)) :-
    when(
        (   ground([X, Y])
        ),
        (   sub_atom(X, _, _, 0, Y)
        )
    ).

'<http://www.w3.org/2000/10/swap/string#equalIgnoringCase>'(literal(X, _), literal(Y, _)) :-
    when(
        (   ground([X, Y])
        ),
        (   downcase_atom(X, U),
            downcase_atom(Y, U)
        )
    ).

'<http://www.w3.org/2000/10/swap/string#format>'([literal(A, _)|B], literal(C, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
    when(
        (   ground([A, B])
        ),
        (   atom_codes(A, D),
            subst([[[0'%, 0's], [0'~, 0'w]]], D, E),
            preformat(B, F),
            format_to_chars(E, F, G),
            atom_codes(C, G)
        )
    ).

'<http://www.w3.org/2000/10/swap/string#greaterThan>'(X, Y) :-
    when(
        (   ground([X, Y])
        ),
        (   getstring(X, U),
            getstring(Y, V),
            U @> V
        )
    ).

'<http://www.w3.org/2000/10/swap/string#join>'([X, Y], Z) :-
    when(
        (   ground([X, Y])
        ;   ground(Z)
        ),
        (   ground([X, Y]),
            getlist(X, C),
            getcodes(Y, D),
            labelvars(C, 0, _, avar),
            (   member(E, C),
                var(E),
                var(Z)
            ->  true
            ;   findall([D, S],
                    (   member(A, X),
                        getcodes(A, S)
                    ),
                    U
                ),
                flatten(U, V),
                (   V = []
                ->  F = V
                ;   append(D, F, V)
                ),
                atom_codes(G, F),
                Z = literal(G, type('<http://www.w3.org/2001/XMLSchema#string>'))
            )
        ;   ground(Z),
            getcodes(Z, U),
            getcodes(Y, C),
            escape_string(T, U),
            escape_string(V, C),
            (   V = []
            ->  findall(literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')),
                    (   member(B, T),
                        atom_codes(A, [B])
                    ),
                    Q
                )
            ;   esplit_string(T, V, [], W),
                findall(literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')),
                    (   member(B, W),
                        atom_codes(A, B)
                    ),
                    Q
                )
            ),
            findall(literal(N, type('<http://www.w3.org/2001/XMLSchema#string>')),
                (   member(literal(M, type('<http://www.w3.org/2001/XMLSchema#string>')), Q),
                    atom_codes(M, Mc),
                    escape_string(Mc, Nc),
                    atom_codes(N, Nc)
                ),
                X
            )
        )
    ).

'<http://www.w3.org/2000/10/swap/string#lessThan>'(X, Y) :-
    when(
        (   ground([X, Y])
        ),
        (   getstring(X, U),
            getstring(Y, V),
            U @< V
        )
    ).

'<http://www.w3.org/2000/10/swap/string#length>'(literal(A, _), B) :-
    when(
        (   ground(A)
        ),
        (   escape_atom(A, C),
            sub_atom(C, 0, B, 0, _)
        )
    ).

'<http://www.w3.org/2000/10/swap/string#lowerCase>'(literal(X, Z), literal(Y, Z)) :-
    when(
        (   ground(X)
        ),
        (   downcase_atom(X, Y)
        )
    ).

'<http://www.w3.org/2000/10/swap/string#matches>'(literal(X, _), literal(Y, _)) :-
    when(
        (   ground([X, Y])
        ),
        (   regex(Y, X, _)
        )
    ).

'<http://www.w3.org/2000/10/swap/string#notContainsRoughly>'(X, Y) :-
    \+'<http://www.w3.org/2000/10/swap/string#containsRoughly>'(X, Y).

'<http://www.w3.org/2000/10/swap/string#notEqualIgnoringCase>'(X, Y) :-
    \+'<http://www.w3.org/2000/10/swap/string#equalIgnoringCase>'(X, Y).

'<http://www.w3.org/2000/10/swap/string#notGreaterThan>'(X, Y) :-
    when(
        (   ground([X, Y])
        ),
        (   getstring(X, U),
            getstring(Y, V),
            U @=< V
        )
    ).

'<http://www.w3.org/2000/10/swap/string#notLessThan>'(X, Y) :-
    when(
        (   ground([X, Y])
        ),
        (   getstring(X, U),
            getstring(Y, V),
            U @>= V
        )
    ).

'<http://www.w3.org/2000/10/swap/string#notMatches>'(X, Y) :-
    \+'<http://www.w3.org/2000/10/swap/string#matches>'(X, Y).

'<http://www.w3.org/2000/10/swap/string#replace>'([literal(X, _), literal(Search, _), literal(Replace, _)], literal(Y, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
    when(
        (   ground([X, Search, Replace])
        ),
        (   (   atomic_list_concat(['(', Search, ')'], Se),
                regex(Se, X, [St|_]),
                escape_atom(S, St)
            ->  (   sub_atom(Search, 0, 1, _, '^')
                ->  atom_concat(S, T, X),
                    atom_concat(Replace, T, Y)
                ;   (   sub_atom(Search, _, 1, 0, '$')
                    ->  atom_concat(T, S, X),
                        atom_concat(T, Replace, Y)
                    ;   atom_codes(X, XC),
                        string_codes(S, SC),
                        atom_codes(Replace, RC),
                        subst([[[0'$, 0'1], SC]], RC, TC),
                        subst([[SC, TC]], XC, YC),
                        atom_codes(Y, YC)
                    )
                )
            ;   Y = X
            )
        )
    ).

'<http://www.w3.org/2000/10/swap/string#replaceAll>'([literal(X, _), SearchList, ReplaceList], literal(Y, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
    when(
        (   ground([X, SearchList, ReplaceList])
        ),
        (   preformat(SearchList, SearchList2),
            preformat(ReplaceList, ReplaceList2),
            replace(SearchList2, ReplaceList2, X, Z),
            atom_string(Y, Z)
        )
    ).

'<http://www.w3.org/2000/10/swap/string#scrape>'([literal(X, _), literal(Y, _)], literal(Z, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
    when(
        (   ground([X, Y])
        ),
        (   regex(Y, X, [W|_]),
            atom_string(V, W),
            escape_atom(Z, V)
        )
    ).

'<http://www.w3.org/2000/10/swap/string#scrapeAll>'([literal(X, _), literal(Y, _)], Z) :-
    when(
        (   ground([X, Y])
        ),
        (   scrape(X, Y, V),
            preformat(Z, V)
        )
    ).

'<http://www.w3.org/2000/10/swap/string#search>'([literal(X, _), literal(Y, _)], Z) :-
    when(
        (   ground([X, Y])
        ),
        (   regex(Y, X, L),
            findall(literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')),
                (   member(M, L),
                    atom_string(A, M)
                ),
                Z
            )
        )
    ).

'<http://www.w3.org/2000/10/swap/string#startsWith>'(literal(X, _), literal(Y, _)) :-
    when(
        (   ground([X, Y])
        ),
        (   sub_atom(X, 0, _, _, Y)
        )
    ).

'<http://www.w3.org/2000/10/swap/string#substring>'([literal(A, _), B, C], literal(D, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
    !,
    when(
        (   ground([A, B, C])
        ),
        (   getint(B, I),
            getint(C, J),
            (   I < 1
            ->  G is 0,
                H is J+I-1
            ;   G is I-1,
                H is J
            ),
            (   H < 0
            ->  D = ''
            ;   sub_atom(A, G, H, _, D)
            )
        )
    ).
'<http://www.w3.org/2000/10/swap/string#substring>'([literal(A, _), B], literal(D, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
    when(
        (   ground([A, B])
        ),
        (   getint(B, I),
            sub_atom(A, 0, E, 0, _),
            J is E-I+1,
            (   I < 1
            ->  G is 0,
                H is J+I-1
            ;   G is I-1,
                H is J
            ),
            (   H < 0
            ->  D = []
            ;   sub_atom(A, G, H, _, D)
            )
        )
    ).

'<http://www.w3.org/2000/10/swap/string#upperCase>'(literal(X, Z), literal(Y, Z)) :-
    when(
        (   ground(X)
        ),
        (   upcase_atom(X, Y)
        )
    ).

'<http://www.w3.org/2000/10/swap/time#day>'(literal(X, _), literal(Y, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
    when(
        (   ground(X)
        ),
        (   sub_atom(X, 8, 2, _, Y)
        )
    ).

'<http://www.w3.org/2000/10/swap/time#localTime>'(literal(X, _), literal(Y, type('<http://www.w3.org/2001/XMLSchema#dateTime>'))) :-
    when(
        (   ground(X)
        ),
        (   timestamp(Y)
        )
    ).

'<http://www.w3.org/2000/10/swap/time#month>'(literal(X, _), literal(Y, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
    when(
        (   ground(X)
        ),
        (   sub_atom(X, 5, 2, _, Y)
        )
    ).

'<http://www.w3.org/2000/10/swap/time#year>'(literal(X, _), literal(Y, type('<http://www.w3.org/2001/XMLSchema#string>'))) :-
    when(
        (   ground(X)
        ),
        (   sub_atom(X, 0, 4, _, Y)
        )
    ).

%
% Support
%

put_pfx(_, URI) :-
    atomic_list_concat(['<', URI, '>'], U),
    pfx(_, U),
    !.
put_pfx(Pf, URI) :-
    atomic_list_concat(['<', URI, '>'], U),
    fresh_pf(Pf, Pff),
    assertz(pfx(Pff, U)).

fresh_pf(Pf, Pfx) :-
    atom_concat(Pf, ':', Pfx),
    \+pfx(Pfx, _),
    !.
fresh_pf(_, Pfx) :-
    gensym(ns, Pfn),
    fresh_pf(Pfn, Pfx).

cnt(A) :-
    nb_getval(A, B),
    C is B+1,
    nb_setval(A, C).

cnt(A, I) :-
    nb_getval(A, B),
    C is B+I,
    nb_setval(A, C).

within_scope([A, B]) :-
    (   var(B)
    ->  B = 1
    ;   true
    ),
    (   B = 0
    ->  brake
    ;   nb_getval(limit, C),
        (   C < B
        ->  nb_setval(limit, B)
        ;   true
        ),
        recursion(B)
    ),
    nb_getval(scope, A).

exopred(P, S, O) :-
    (   var(P),
        var(S),
        var(O)
    ->  pred(P),
        H =.. [P, S, O],
        clause(H, true)
    ;   (   var(P)
        ->  pred(P)
        ;   atom(P),
            current_predicate(P/2)
        ),
        call(P, S, O)
    ).

exogen :-
    forall(
        (   clause(exopred(P, S, O), Body),
            (   nonvar(S)
            ;   nonvar(O)
            )
        ),
        (   (   var(P)
            ->  pred(P)
            ;   atom(P),
                current_predicate(P/2)
            ),
            Head =.. [P, S, O],
            (   \+clause(Head, Body)
            ->  assertz(Head :- Body)
            ;   true
            )
        )
    ).
exogen.

ucall(A) :-
    (   A = (B, C)
    ->  vcall(B),
        ucall(C)
    ;   vcall(A)
    ).

vcall(A) :-
    (   (   A =.. [_, V, W]
        ;   A = exopred(_, V, W)
        ),
        (   is_gl(V)
        ;   is_gl(W)
        )
    ->  unify(A, B)
    ;   B = A
    ),
    (   B =.. [C, D, E],
        pred(C),
        (   is_gl(D)
        ;   is_gl(E)
        )
    ->  G =.. [C, H, I],
        call(G),
        unify(H, D),
        unify(I, E)
    ;   call(B)
    ).

is_gl(A) :-
    nonvar(A),
    \+is_list(A),
    (   A = (_, _),
        !
    ;   A =.. [F, _, _],
        F \= literal,
        F \= rdiv,
        !
    ;   A = exopred(_, _, _)
    ).

is_graph(true).
is_graph(A) :-
    is_gl(A).

unify(A, B) :-
    nonvar(A),
    A = exopred(P, S, O),
    (   (   nonvar(B)
        ;   nonvar(P)
        )
    ->  (   nonvar(P)
        ->  atom(P)
        ;   true
        ),
        B =.. [P, T, R],
        atom(P),
        unify(S, T),
        unify(O, R)
    ;   B = exopred(P, T, R),
        unify(S, T),
        unify(O, R)
    ),
    !.
unify(A, B) :-
    nonvar(B),
    B = exopred(P, S, O),
    (   (   nonvar(A)
        ;   nonvar(P)
        )
    ->  (   nonvar(P)
        ->  atom(P)
        ;   true
        ),
        A =.. [P, T, R],
        atom(P),
        unify(S, T),
        unify(O, R)
    ;   A = exopred(P, T, R),
        unify(S, T),
        unify(O, R)
    ),
    !.
unify(A, B) :-
    is_list(A),
    !,
    getlist(B, C),
    C = A.
unify(A, B) :-
    is_list(B),
    !,
    getlist(A, C),
    C = B.
unify(A, B) :-
    nonvar(A),
    nonvar(B),
    (   A = (_, _)
    ;   B = (_, _)
    ),
    !,
    conj_list(A, C),
    conj_list(B, D),
    includes(C, D),
    includes(D, C).
unify(A, B) :-
    nonvar(A),
    nonvar(B),
    A =.. [P, S, O],
    B =.. [P, T, R],
    !,
    unify(S, T),
    unify(O, R).
unify(A, A).

conj_list(true, []) :-
    !.
conj_list(A, [A]) :-
    A \= (_, _),
    A \= false,
    !.
conj_list((A, B), [A|C]) :-
    conj_list(B, C).

conj_append(A, true, A) :-
    !.
conj_append((A, B), C, (A, D)) :-
    conj_append(B, C, D),
    !.
conj_append(A, B, (A, B)).

move_rdf([], []).
move_rdf(['<http://www.w3.org/1999/02/22-rdf-syntax-ns#first>'(A, B)|C], D) :-
    !,
    assertz('<http://www.w3.org/1999/02/22-rdf-syntax-ns#first>'(A, B)),
    move_rdf(C, D).
move_rdf(['<http://www.w3.org/1999/02/22-rdf-syntax-ns#rest>'(A, B)|C], D) :-
    !,
    assertz('<http://www.w3.org/1999/02/22-rdf-syntax-ns#rest>'(A, B)),
    move_rdf(C, D).
move_rdf(['<http://www.w3.org/1999/02/22-rdf-syntax-ns#value>'(A, B)|C], D) :-
    !,
    assertz('<http://www.w3.org/1999/02/22-rdf-syntax-ns#value>'(A, B)),
    move_rdf(C, D).
move_rdf([A|B], [A|C]) :-
    move_rdf(B, C).

includes(_, []) :-
    !.
includes(X, [Y|Z]) :-
    member(U, X),
    unify(U, Y),
    includes(X, Z).

conjoin([X], X) :-
    !.
conjoin([true|Y], Z) :-
    conjoin(Y, Z),
    !.
conjoin([X|Y], Z) :-
    conjoin(Y, U),
    conj_append(X, U, V),
    (   ground(V)
    ->  conj_list(V, A),
        list_to_set(A, B),
        conj_list(Z, B)
    ;   Z = V
    ).

difference([true, _], true) :-
    !.
difference([X, true], X) :-
    !.
difference([X, Y], Z) :-
    conj_list(X, U),
    conj_list(Y, V),
    difference(U, V, W),
    distinct(W, J),
    conj_list(Z, J).

difference([], _, []) :-
    !.
difference([X|Y], U, V) :-
    member(Z, U),
    unify(X, Z),
    !,
    difference(Y, U, V).
difference([X|Y], U, [X|V]) :-
    difference(Y, U, V).

intersect([X], X) :-
    !.
intersect([true|_], true) :-
    !.
intersect([X|Y], Z) :-
    intersect(Y, I),
    conj_list(X, U),
    conj_list(I, V),
    intersect(U, V, W),
    distinct(W, J),
    conj_list(Z, J),
    !.

intersect([], _, []) :-
    !.
intersect([X|Y], U, V) :-
    member(Z, U),
    unify(X, Z),
    V = [X|W],
    intersect(Y, U, W).
intersect([_|Y], U, V) :-
    intersect(Y, U, V).

distinct(A, B) :-
    (   ground(A)
    ->  distinct_hash(A, B)
    ;   distinct_value(A, B)
    ).

distinct_hash([], []) :-
    (   retract(hash_value(_, _)),
        fail
    ;   true
    ),
    !.
distinct_hash([A|B], C) :-
    term_hash(A, D),
    (   hash_value(D, E)
    ->  (   unify(A, E)
        ->  C = F
        ;   C = [A|F]
        )
    ;   assertz(hash_value(D, A)),
        C = [A|F]
    ),
    distinct_hash(B, F).

distinct_value([], []).
distinct_value([A|B], [A|D]) :-
    nonvar(A),
    !,
    del(B, A, E),
    distinct_value(E, D).
distinct_value([_|A], B) :-
    distinct_value(A, B).

del([], _, []).
del([A|B], C, D) :-
    copy_term_nat(A, Ac),
    copy_term_nat(C, Cc),
    unify(Ac, Cc),
    !,
    del(B, C, D).
del([A|B], C, [A|D]) :-
    del(B, C, D).

subst(_, [], []).
subst(A, B, C) :-
    member([D, E], A),
    append(D, F, B),
    !,
    append(E, G, C),
    subst(A, F, G).
subst(A, [B|C], [B|D]) :-
    subst(A, C, D).

replace([], [], X, X) :-
    !.
replace([Search|SearchRest], [Replace|ReplaceRest], X, Y) :-
    atomic_list_concat(['(', Search, ')'], Scap),
    scrape(X, Scap, Scrape),
    atom_codes(Replace, RC),
    srlist(Scrape, RC, Subst),
    atom_codes(X, XC),
    subst(Subst, XC, ZC),
    atom_codes(Z, ZC),
    replace(SearchRest, ReplaceRest, Z, Y).

scrape(X, Y, [V|Z]) :-
    regex(Y, X, [W|_]),
    atom_string(V, W),
    sub_atom(X, _, _, I, V),
    sub_atom(X, _, I, 0, U),
    !,
    scrape(U, Y, Z).
scrape(_, _, []).

srlist([], _, []).
srlist([A|B], C, [[E, C]|D]) :-
    string_codes(A, E),
    srlist(B, C, D).

sum([], 0) :-
    !.
sum([A|B], C) :-
    getnumber(A, X),
    sum(B, D),
    C is X+D.

product([], 1) :-
    !.
product([A|B], C) :-
    getnumber(A, X),
    product(B, D),
    C is X*D.

if_then_else(A, B, C) :-
    (   catch(call(A), _, fail)
    ->  catch(call(B), _, fail)
    ;   catch(call(C), _, fail)
    ).

':-'(A, B) :-
    (   var(A)
    ->  cpred(C),
        A =.. [C, _, _]
    ;   true
    ),
    (   A = exopred(P, S, O)
    ->  Ax =.. [P, S, O]
    ;   Ax = A
    ),
    clause(Ax, B).

sub_atom_last(A, B, C, D, E) :-
    sub_atom(A, B, C, D, E),
    F is B+1,
    sub_atom(A, F, _, 0, G),
    (   sub_atom(G, _, C, _, E)
    ->  sub_atom_last(G, B, C, D, E)
    ;   true
    ).

escape_atom(A, B) :-
    ground(A),
    !,
    atom_codes(A, C),
    escape_codes(D, C),
    atom_codes(B, D).
escape_atom(A, B) :-
    ground(B),
    atom_codes(B, C),
    escape_codes(C, D),
    atom_codes(A, D).

escape_string([], []) :-
    !.
escape_string([0'\t|A], [0'\\, 0't|B]) :-
    !,
    escape_string(A, B).
escape_string([0'\b|A], [0'\\, 0'b|B]) :-
    !,
    escape_string(A, B).
escape_string([0'\n|A], [0'\\, 0'n|B]) :-
    !,
    escape_string(A, B).
escape_string([0'\r|A], [0'\\, 0'r|B]) :-
    !,
    escape_string(A, B).
escape_string([0'\f|A], [0'\\, 0'f|B]) :-
    !,
    escape_string(A, B).
escape_string([0'"|A], [0'\\, 0'"|B]) :-
    !,
    escape_string(A, B).
escape_string([0'\\|A], [0'\\, 0'\\|B]) :-
    !,
    escape_string(A, B).
escape_string([A|B], [A|C]) :-
    escape_string(B, C).

escape_unicode([], []) :-
    !.
escape_unicode([A, B|C], D) :-
    0xD800 =< A,
    A =< 0xDBFF,
    0xDC00 =< B,
    B =< 0xDFFF,
    E is 0x10000+(A-0xD800)*0x400+(B-0xDC00),
    (   0x100000 =< E
    ->  with_output_to(codes(F), format('\\U00~16R', [E]))
    ;   with_output_to(codes(F), format('\\U000~16R', [E]))
    ),
    append(F, G, D),
    !,
    escape_unicode(C, G).
escape_unicode([A|B], [A|C]) :-
    escape_unicode(B, C).

esplit_string([], _, [], []) :-
    !.
esplit_string([], _, A, [A]) :-
    !.
esplit_string([A|B], C, [], D) :-
    memberchk(A, C),
    !,
    esplit_string(B, C, [], D).
esplit_string([A|B], C, D, [D|E]) :-
    memberchk(A, C),
    !,
    esplit_string(B, C, [], E).
esplit_string([A|B], C, D, E) :-
    append(D, [A], F),
    esplit_string(B, C, F, E).

quant(A, some) :-
    var(A),
    !.
quant('<http://www.w3.org/2000/10/swap/log#implies>'(_, _), allv) :-
    !.
quant(':-'(_, _), allv) :-
    !.
quant(answer('<http://www.w3.org/2000/10/swap/log#implies>', _, _), allv) :-
    !.
quant(answer(':-', _, _), allv) :-
    !.
quant(_-A, avar) :-
    conj_list(A, B),
    member('<http://www.w3.org/2000/10/swap/log#implies>'(_, _), B),
    !.
quant(_, some).

labelvars(A, B, C) :-
    quant(A, Q),
    labelvars(A, B, C, Q).

labelvars(A, B, C, D) :-
    var(A),
    !,
    atom_number(E, B),
    atomic_list_concat([D, E], A),      % failing when A is an attributed variable
    C is B+1.
labelvars(A, B, B, _) :-
    atomic(A),
    !.
labelvars('<http://www.w3.org/2000/10/swap/log#implies>'(A, B), C, C, D) :-
    D \= avar,
    nonvar(A),
    nonvar(B),
    !.
labelvars((A, B), C, D, Q) :-
    !,
    labelvars(A, C, E, Q),
    labelvars(B, E, D, Q).
labelvars([A|B], C, D, Q) :-
    !,
    labelvars(A, C, E, Q),
    labelvars(B, E, D, Q).
labelvars(A, B, C, Q) :-
    nonvar(A),
    functor(A, _, D),
    labelvars(0, D, A, B, C, Q).

labelvars(A, A, _, B, B, _) :-
    !.
labelvars(A, B, C, D, E, Q) :-
    F is A+1,
    arg(F, C, G),
    labelvars(G, D, H, Q),
    labelvars(F, B, C, H, E, Q).

relabel(A, A) :-
    var(A),
    !,
    nb_getval(wn, W),
    labelvars(A, W, N, allv),
    nb_setval(wn, N).
relabel([], []) :-
    !.
relabel([A|B], [C|D]) :-
    !,
    relabel(A, C),
    relabel(B, D).
relabel(A, A) :-
    atom(A),
    !.
relabel(A, A) :-
    number(A),
    !.
relabel(A, B) :-
    A =.. [C|D],
    relabel(C, E),
    relabel(D, F),
    B =.. [E|F].

conjify((A, B), (C, D)) :-
    !,
    conjify(A, C),
    conjify(B, D).
conjify('<http://www.w3.org/2000/10/swap/log#callWithCut>'(true, true), !) :-
    !.
conjify(A, A).

contains(X, X) :-
    !.
contains(X, Term) :-
    compound(Term),
    arg(_, Term, Arg),
    contains(X, Arg),
    !.

commonvars(A, B, C) :-
    term_variables(A, D),
    term_variables(B, E),
    copy_term_nat([D, E], [F, G]),
    labelvars([F, G], 0, _),
    findall(H,
        (   member(H, F),
            member(H, G)
        ),
        C
    ).

makevars(A, B, alpha(C)) :-
    !,
    distinct(C, D),
    findvars(A, Z, zeta),
    append(D, Z, E),
    findall([X, Y],
        (   member(X, E),
            (   sub_atom(X, _, 1, I, '#')
            ->  J is I-1,
                sub_atom(X, _, J, 1, Q)
            ;   Q = X
            ),
            atomic_list_concat(['<http://www.w3.org/2000/10/swap/var#', Q, '>'], Y)
        ),
        F
    ),
    makevar(A, B, F).
makevars(A, B, beta(C)) :-
    !,
    distinct(C, D),
    findvars(A, Z, zeta),
    append(D, Z, E),
    findall([X, _],
        (   member(X, E)
        ),
        F
    ),
    makevar(A, B, F).
makevars(A, B, Z) :-
    findvars(A, C, Z),
    distinct(C, D),
    findall([X, _],
        (   member(X, D)
        ),
        F
    ),
    makevar(A, B, F).

makevar(A, B, D) :-
    atomic(A),
    !,
    (   atom(A),
        member([A, B], D)
    ->  true
    ;   B = A
    ).
makevar(A, A, _) :-
    var(A),
    !.
makevar([], [], _) :-
    !.
makevar([A|B], [C|D], F) :-
    makevar(A, C, F),
    makevar(B, D, F),
    !.
makevar(A, B, F) :-
    A =.. C,
    makevar(C, [Dh|Dt], F),
    (   nonvar(Dh)
    ->  B =.. [Dh|Dt]
    ;   Dt = [Ds, Do],
        B = exopred(Dh, Ds, Do)
    ).

findvars(A, B, Z) :-
    atomic(A),
    !,
    (   atom(A),
        findvar(A, Z)
    ->  B = [A]
    ;   B = []
    ).
findvars(A, [], _) :-
    var(A),
    !.
findvars([], [], _) :-
    !.
findvars([A|B], C, Z) :-
    !,
    findvars(A, D, Z),
    findvars(B, E, Z),
    append(D, E, C).
findvars(A, B, Z) :-
    A =.. C,
    findvars(C, B, Z).

findvar(A, alpha) :-
    !,
    (   atom_concat('<http://www.w3.org/2000/10/swap/var#', _, A)
    ;   sub_atom(A, 0, _, _, avar)
    ).
findvar(A, beta) :-
    !,
    (   sub_atom(A, 0, _, _, '_bn_')
    ;   sub_atom(A, 0, _, _, '_e_')
    ;   sub_atom(A, _, 4, _, '#qe_')
    ;   sub_atom(A, _, 19, _, '/.well-known/genid/')
    ;   sub_atom(A, 0, _, _, some)
    ;   sub_atom(A, 0, _, _, '_:')
    ).
findvar(A, delta) :-
    !,
    (   sub_atom(A, _, 19, _, '/.well-known/genid/')
    ;   sub_atom(A, 0, _, _, some)
    ).
findvar(A, zeta) :-
    !,
    (   sub_atom(A, _, 19, _, '/.well-known/genid/'),
        (   sub_atom(A, _, 4, _, '#bn_')
        ;   sub_atom(A, _, 4, _, '#e_')
        )
    ;   sub_atom(A, 0, _, _, some)
    ;   sub_atom(A, 0, _, _, avar)
    ).
findvar(A, eta) :-
    sub_atom(A, 0, _, _, allv).

raw_type(A, '<http://www.w3.org/2000/10/swap/log#ForAll>') :-
    var(A),
    !.
raw_type(A, '<http://www.w3.org/1999/02/22-rdf-syntax-ns#List>') :-
    is_list(A),
    !.
raw_type(A, '<http://www.w3.org/2000/10/swap/log#Literal>') :-
    number(A),
    !.
raw_type(true, '<http://www.w3.org/2000/10/swap/log#Formula>').
raw_type(false, '<http://www.w3.org/2000/10/swap/log#Formula>').
raw_type(A, '<http://www.w3.org/2000/10/swap/log#Literal>') :-
    atom(A),
    \+ sub_atom(A, 0, _, _, some),
    \+ sub_atom(A, 0, _, _, avar),
    \+ (sub_atom(A, 0, 1, _, '<'), sub_atom(A, _, 1, 0, '>')),
    !.
raw_type(literal(_, _), '<http://www.w3.org/2000/10/swap/log#Literal>') :-
    !.
raw_type(rdiv(_, _), '<http://www.w3.org/2000/10/swap/log#Literal>') :-
    !.
raw_type((_, _), '<http://www.w3.org/2000/10/swap/log#Formula>') :-
    !.
raw_type(A, '<http://www.w3.org/2000/10/swap/log#Formula>') :-
    functor(A, B, C),
    B \= ':',
    C >= 2,
    !.
raw_type(A, '<http://www.w3.org/2000/10/swap/log#UnlabeledBlankNode>') :-
    nb_getval(var_ns, B),
    sub_atom(A, 1, _, _, B),
    sub_atom(A, _, 4, _, '#bn_'),
    !.
raw_type(A, '<http://www.w3.org/2000/10/swap/log#LabeledBlankNode>') :-
    nb_getval(var_ns, B),
    sub_atom(A, 1, _, _, B),
    sub_atom(A, _, 3, _, '#e_'),
    !.
raw_type(A, '<http://www.w3.org/2000/10/swap/log#SkolemIRI>') :-
    sub_atom(A, _, 19, _, '/.well-known/genid/'),
    !.
raw_type(A, '<http://www.w3.org/2000/10/swap/log#ForSome>') :-
    sub_atom(A, 1, _, _, 'http://www.w3.org/2000/10/swap/var#qe_'),
    !.
raw_type(_, '<http://www.w3.org/2000/10/swap/log#Other>').

getnumber(rdiv(A, B), C) :-
    nonvar(A),
    !,
    C is A/B.
getnumber(A, A) :-
    number(A),
    !.
getnumber(A, A) :-
    nonvar(A),
    memberchk(A, [inf, -inf, nan]),
    !.
getnumber(literal(A, type('<http://www.w3.org/2001/XMLSchema#dateTime>')), B) :-
    !,
    ground(A),
    atom_codes(A, C),
    datetime(B, C, []).
getnumber(literal(A, type('<http://www.w3.org/2001/XMLSchema#date>')), B) :-
    !,
    ground(A),
    atom_codes(A, C),
    date(B, C, []).
getnumber(literal(A, type('<http://www.w3.org/2001/XMLSchema#time>')), B) :-
    !,
    ground(A),
    atom_codes(A, C),
    time(B, C, []).
getnumber(literal(A, type('<http://www.w3.org/2001/XMLSchema#duration>')), B) :-
    !,
    ground(A),
    atom_codes(A, C),
    duration(B, C, []).
getnumber(literal(A, type('<http://www.w3.org/2001/XMLSchema#yearMonthDuration>')), B) :-
    !,
    ground(A),
    atom_codes(A, C),
    yearmonthduration(B, C, []).
getnumber(literal(A, type('<http://www.w3.org/2001/XMLSchema#dayTimeDuration>')), B) :-
    !,
    ground(A),
    atom_codes(A, C),
    daytimeduration(B, C, []).
getnumber(literal(A, _), B) :-
    ground(A),
    atom_codes(A, C),
    numeral(C, D),
    catch(number_codes(B, D), _, fail).

getint(A, B) :-
    getnumber(A, C),
    B is integer(round(C)).

getbool(literal(false, type('<http://www.w3.org/2001/XMLSchema#boolean>')), false).
getbool(literal(true, type('<http://www.w3.org/2001/XMLSchema#boolean>')), true).
getbool(literal('0', type('<http://www.w3.org/2001/XMLSchema#boolean>')), false).
getbool(literal('1', type('<http://www.w3.org/2001/XMLSchema#boolean>')), true).
getbool(false, false).
getbool(true, true).

getlist(A, A) :-
    var(A),
    !.
getlist([], []) :-
    !.
getlist('<http://www.w3.org/1999/02/22-rdf-syntax-ns#nil>', []) :-
    !.
getlist([A|B], [C|D]) :-
    getlist(A, C),
    !,
    getlist(B, D).
getlist([A|B], [A|D]) :-
    !,
    getlist(B, D).
getlist(A, [B|C]) :-
    '<http://www.w3.org/1999/02/22-rdf-syntax-ns#first>'(A, B),
    (   '<http://www.w3.org/1999/02/22-rdf-syntax-ns#first>'(A, B2),
        B2 \= B
    ->  throw(malformed_list_extra_first(A, B, B2))
    ;   true
    ),
    (   '<http://www.w3.org/1999/02/22-rdf-syntax-ns#rest>'(A, D),
        (   '<http://www.w3.org/1999/02/22-rdf-syntax-ns#rest>'(A, D2),
            D2 \= D
        ->  throw(malformed_list_extra_rest(A, D, D2))
        ;   true
        )
    ->  true
    ;   throw(malformed_list_no_rest(A))
    ),
    (   getlist(D, C)
    ->  true
    ;   throw(malformed_list_invalid_rest(D))
    ).

getterm(A, A) :-
    var(A),
    !.
getterm(A, B) :-
    '<http://www.w3.org/1999/02/22-rdf-syntax-ns#value>'(A, B),
    !.
getterm([], []) :-
    !.
getterm('<http://www.w3.org/1999/02/22-rdf-syntax-ns#nil>', []) :-
    !.
getterm([A|B], [C|D]) :-
    getterm(A, C),
    !,
    getterm(B, D).
getterm(A, [B|C]) :-
    '<http://www.w3.org/1999/02/22-rdf-syntax-ns#first>'(A, D),
    (   '<http://www.w3.org/1999/02/22-rdf-syntax-ns#first>'(A, D2),
        D2 \= D
    ->  throw(malformed_list_extra_first(A, D, D2))
    ;   true
    ),
    (   '<http://www.w3.org/1999/02/22-rdf-syntax-ns#rest>'(A, E),
        (   '<http://www.w3.org/1999/02/22-rdf-syntax-ns#rest>'(A, E2),
            E2 \= E
        ->  throw(malformed_list_extra_rest(A, E, E2))
        ;   true
        )
    ->  true
    ;   throw(malformed_list_no_rest(A))
    ),
    !,
    getterm(D, B),
    (   getterm(E, C),
        is_list(C)
    ->  true
    ;   throw(malformed_list_invalid_rest(E))
    ).
getterm(graph(A, B), graph(A, C)) :-
    graph(A, B),
    !,
    getterm(B, D),
    conjify(D, C).
getterm(graph(A, B), '<http://www.w3.org/2000/10/swap/log#equalTo>'(B, C)) :-
    getconj(A, D),
    D \= A,
    !,
    getterm(D, E),
    conjify(E, C).
getterm(A, B) :-
    graph(A, _),
    !,
    getconj(A, C),
    getterm(C, D),
    conjify(D, B).
getterm(A, B) :-
    A =.. [C|D],
    getterm(D, E),
    B =.. [C|E].

getconj(A, B) :-
    nonvar(A),
    findall(C,
        (   graph(A, C)
        ),
        D
    ),
    D \= [],
    !,
    conjoin(D, B).
getconj(A, A).

getstring(A, B) :-
    '<http://www.w3.org/2000/10/swap/log#uri>'(A, B),
    !.
getstring(A, A).

getcodes(literal(A, _), B) :-
    nonvar(A),
    !,
    atom_codes(A, B).
getcodes(A, B) :-
    nonvar(A),
    with_output_to_chars(wg(A), B).

remember(A) :-
    \+call(A),
    !,
    assertz(A).
remember(_).

preformat([], []) :-
    !.
preformat([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>'))|B], [A|D]) :-
    !,
    preformat(B, D).
preformat([A|B], [A|D]) :-
    preformat(B, D).

numeral([0'-, 0'.|A], [0'-, 0'0, 0'.|A]) :-
    !.
numeral([0'+, 0'.|A], [0'+, 0'0, 0'.|A]) :-
    !.
numeral([0'.|A], [0'0, 0'.|A]) :-
    !.
numeral(A, B) :-
    append([C, [0'., 0'e], D], A),
    append([C, [0'., 0'0, 0'e], D], B),
    !.
numeral(A, B) :-
    append([C, [0'., 0'E], D], A),
    append([C, [0'., 0'0, 0'E], D], B),
    !.
numeral(A, B) :-
    last(A, 0'.),
    append(A, [0'0], B),
    !.
numeral(A, A).

lzero([], []) :-
    !.
lzero([_|A], [0'0|B]) :-
    lzero(A, B).

dtlit([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')), C], B) :-
    nonvar(C),
    '<http://www.w3.org/2000/01/rdf-schema#subClassOf>'(C, '<http://www.w3.org/2001/XMLSchema#integer>'),
    integer(B),
    !,
    atom_number(A, B).
dtlit([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')), '<http://www.w3.org/2001/XMLSchema#integer>'], B) :-
    integer(B),
    !,
    atom_number(A, B).
dtlit([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')), '<http://www.w3.org/2001/XMLSchema#double>'], B) :-
    float(B),
    !,
    atom_number(A, B).
dtlit([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')), '<http://www.w3.org/2001/XMLSchema#dateTime>'], B) :-
    (   number(B)
    ->  datetime(B, C)
    ;   nonvar(B),
        B = date(Year, Month, Day, Hour, Minute, Second, Offset, _, _),
        datetime(Year, Month, Day, Hour, Minute, Second, Offset, C)
    ),
    !,
    atom_codes(A, C).
dtlit([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')), '<http://www.w3.org/2001/XMLSchema#date>'], B) :-
    (   number(B)
    ->  date(B, C)
    ;   nonvar(B),
        B = date(Year, Month, Day, _, _, _, Offset, _, _),
        date(Year, Month, Day, Offset, C)
    ),
    !,
    atom_codes(A, C).
dtlit([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')), '<http://www.w3.org/2001/XMLSchema#time>'], B) :-
    (   number(B)
    ->  time(B, C)
    ;   nonvar(B),
        B = date(_, _, _, Hour, Minute, Second, Offset, _, _),
        time(Hour, Minute, Second, Offset, C)
    ),
    !,
    atom_codes(A, C).
dtlit([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')), '<http://www.w3.org/2001/XMLSchema#duration>'], B) :-
    number(B),
    !,
    daytimeduration(B, C),
    atom_codes(A, C).
dtlit([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')), '<http://www.w3.org/2001/XMLSchema#yearMonthDuration>'], B) :-
    number(B),
    !,
    yearmonthduration(B, C),
    atom_codes(A, C).
dtlit([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')), '<http://www.w3.org/2001/XMLSchema#dayTimeDuration>'], B) :-
    number(B),
    !,
    daytimeduration(B, C),
    atom_codes(A, C).
dtlit([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')), '<http://www.w3.org/2001/XMLSchema#boolean>'], A) :-
    atomic(A),
    getbool(A, A),
    !.
dtlit([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')), prolog:atom], A) :-
    atomic(A),
    \+ (sub_atom(A, 0, 1, _, '<'), sub_atom(A, _, 1, 0, '>')),
    !.
dtlit([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')), '<http://www.w3.org/1999/02/22-rdf-syntax-ns#langString>'], literal(A, lang(_))) :-
    !.
dtlit([literal(A, type('<http://www.w3.org/2001/XMLSchema#string>')), B], literal(A, type(B))).

datetime(A, B) :-
    stamp_date_time(A, date(Year, Month, Day, Hour, Minute, Second, _, _, _), 0),
    fmsec(A, Second, Sec),
    ycodes(Year, C),
    ncodes(Month, D),
    ncodes(Day, E),
    ncodes(Hour, F),
    ncodes(Minute, G),
    ncodes(Sec, H),
    append([C, [0'-], D, [0'-], E, [0'T], F, [0':], G, [0':], H, [0'Z]], B).

datetime(A, L1, L13) :-
    int(B, L1, [0'-|L3]),
    int(C, L3, [0'-|L5]),
    int(D, L5, [0'T|L7]),
    int(E, L7, [0':|L9]),
    int(F, L9, [0':|L11]),
    decimal(G, L11, L12),
    timezone(H, L12, L13),
    I is -H,
    catch(date_time_stamp(date(B, C, D, E, F, G, I, -, -), J), _, fail),
    fmsec(G, J, A).

datetime(Year, Month, Day, Hour, Minute, Second, Offset, B) :-
    ycodes(Year, C),
    ncodes(Month, D),
    ncodes(Day, E),
    ncodes(Hour, F),
    ncodes(Minute, G),
    ncodes(Second, H),
    (   Offset =:= 0
    ->  append([C, [0'-], D, [0'-], E, [0'T], F, [0':], G, [0':], H, [0'Z]], B)
    ;   (   Offset > 0
        ->  I = [0'-],
            OHour is Offset//3600
        ;   I = [0'+],
            OHour is -Offset//3600
        ),
        ncodes(OHour, J),
        OMinute is (Offset mod 3600)//60,
        ncodes(OMinute, K),
        append([C, [0'-], D, [0'-], E, [0'T], F, [0':], G, [0':], H, I, J, [0':], K], B)
    ).

date(A, B) :-
    N is A+3600*12,
    stamp_date_time(N, date(Year, Month, Day, _, _, _, _, _, _), 0),
    ycodes(Year, C),
    ncodes(Month, D),
    ncodes(Day, E),
    Offset is (round(floor(N)) mod 86400) - 3600*12,
    (   Offset =:= 0
    ->  append([C, [0'-], D, [0'-], E, [0'Z]], B)
    ;   (   Offset > 0
        ->  I = [0'-],
            OHour is Offset//3600
        ;   I = [0'+],
            OHour is -Offset//3600
        ),
        ncodes(OHour, J),
        OMinute is (Offset mod 3600)//60,
        ncodes(OMinute, K),
        append([C, [0'-], D, [0'-], E, I, J, [0':], K], B)
    ).

date(A, L1, L7) :-
    int(B, L1, [0'-|L3]),
    int(C, L3, [0'-|L5]),
    int(D, L5, L6),
    timezone(H, L6, L7),
    I is -H,
    catch(date_time_stamp(date(B, C, D, 0, 0, 0, I, -, -), E), _, fail),
    fmsec(0, E, A).

date(Year, Month, Day, Offset, B) :-
    ycodes(Year, C),
    ncodes(Month, D),
    ncodes(Day, E),
    (   Offset =:= 0
    ->  append([C, [0'-], D, [0'-], E, [0'Z]], B)
    ;   (   Offset > 0
        ->  I = [0'-],
            OHour is Offset//3600
        ;   I = [0'+],
            OHour is -Offset//3600
        ),
        ncodes(OHour, J),
        OMinute is (Offset mod 3600)//60,
        ncodes(OMinute, K),
        append([C, [0'-], D, [0'-], E, I, J, [0':], K], B)
    ).

time(A, B) :-
    stamp_date_time(A, date(_, _, _, Hour, Minute, Second, _, _, _), 0),
    fmsec(A, Second, Sec),
    ncodes(Hour, F),
    ncodes(Minute, G),
    ncodes(Sec, H),
    append([F, [0':], G, [0':], H, [0'Z]], B).

time(A, L1, L7) :-
    int(B, L1, [0':|L3]),
    int(C, L3, [0':|L5]),
    decimal(D, L5, L6),
    timezone(E, L6, L7),
    (   B = 24
    ->  A is C*60+D-E
    ;   A is B*3600+C*60+D-E
    ).

time(Hour, Minute, Second, Offset, B) :-
    ncodes(Hour, F),
    ncodes(Minute, G),
    ncodes(Second, H),
    (   Offset =:= 0
    ->  append([F, [0':], G, [0':], H, [0'Z]], B)
    ;   (   Offset > 0
        ->  I = [0'-],
            OHour is Offset//3600
        ;   I = [0'+],
            OHour is -Offset//3600
        ),
        ncodes(OHour, J),
        OMinute is (Offset mod 3600)//60,
        ncodes(OMinute, K),
        append([F, [0':], G, [0':], H, I, J, [0':], K], B)
    ).

duration(A, L1, L7) :-
    dsign(B, L1, [0'P|L3]),
    years(C, L3, L4),
    months(D, L4, L5),
    days(E, L5, L6),
    dtime(F, L6, L7),
    A is B*(C*31556952+D*2629746+E*86400.0+F).

yearmonthduration(A, L1, L5) :-
    dsign(B, L1, [0'P|L3]),
    years(C, L3, L4),
    months(D, L4, L5),
    A is B*(C*12+D).

daytimeduration(A, B) :-
    AInt is round(floor(A)),
    AFrac is A-AInt,
    (   AInt < 0
    ->  C = [0'-]
    ;   C = []
    ),
    D is abs(AInt),
    E is D//86400,
    number_codes(E, Days),
    F is (D-(D//86400)*86400)//3600,
    number_codes(F, Hours),
    G is (D-(D//3600)*3600)//60,
    number_codes(G, Minutes),
    H is D-(D//60)*60+AFrac,
    number_codes(H, Seconds),
    append([C, [0'P| Days], [0'D, 0'T| Hours], [0'H| Minutes], [0'M| Seconds], [0'S]], B).

daytimeduration(A, L1, L5) :-
    dsign(B, L1, [0'P|L3]),
    days(C, L3, L4),
    dtime(D, L4, L5),
    A is B*(C*86400.0+D).

timezone(A, L1, L4) :-
    int(B, L1, [0':|L3]),
    !,
    int(C, L3, L4),
    A is B*3600+C*60.
timezone(0, [0'Z|L2], L2) :-
    !.
timezone(0, L1, L1).

dsign(1, [0'+|L2], L2).
dsign(-1, [0'-|L2], L2).
dsign(1, L1, L1).

dtime(A, [0'T|L2], L5) :-
    !,
    hours(B, L2, L3),
    minutes(C, L3, L4),
    seconds(D, L4, L5),
    A is B*3600+C*60+D.
dtime(0, L1, L1).

years(A, L1, L3) :-
    int(A, L1, [0'Y|L3]).
years(0, L1, L1).

months(A, L1, L3) :-
    int(A, L1, [0'M|L3]).
months(0, L1, L1) .

days(A, L1, L3) :-
    int(A, L1, [0'D|L3]).
days(0, L1, L1).

hours(A, L1, L3) :-
    int(A, L1, [0'H|L3]).
hours(0, L1, L1).

minutes(A, L1, L3) :-
    int(A, L1, [0'M|L3]).
minutes(0, L1, L1).

seconds(A, L1, L3) :-
    decimal(A, L1, [0'S|L3]).
seconds(0, L1, L1).

int(A, L1, L4) :-
    sgn(B, L1, L2),
    digit(C, L2, L3),
    digits(D, L3, L4),
    number_codes(A, [B, C|D]).

decimal(A, L1, L5) :-
    sgn(B, L1, L2),
    digit(C, L2, L3),
    digits(D, L3, L4),
    fraction(E, L4, L5),
    append([B, C|D], E, F),
    number_codes(A, F).

sgn(0'+, [0'+|L2], L2).
sgn(0'-, [0'-|L2], L2).
sgn(0'+, L1, L1).

fraction([0'., A|B], [0'.|L2], L4) :-
    !,
    digit(A, L2, L3),
    digits(B, L3, L4).
fraction([], L1, L1).

digits([A|B], L1, L3) :-
    digit(A, L1, L2),
    digits(B, L2, L3).
digits([], L1, L1).

digit(A, [A|L2], L2) :-
    code_type(A, digit).

fmsec(A, B, C) :-
    integer(A),
    !,
    C is floor(B).
fmsec(_, B, B).

ncodes(A, B) :-
    number_codes(A, D),
    (   A < 10
    ->  B = [0'0| D]
    ;   B = D
    ).

ycodes(A, B) :-
    C is abs(A),
    number_codes(C, D),
    (   C < 10
    ->  E = [0'0, 0'0, 0'0| D]
    ;   (   C < 100
        ->  E = [0'0, 0'0| D]
        ;   (   C < 1000
            ->  E = [0'0| D]
            ;   E = D
            )
        )
    ),
    (   A >= 0
    ->  B = E
    ;   B = [0'-|E]
    ).

absolute_uri('-', '-') :-
    !.
absolute_uri(A, B) :-
    (   is_absolute_url(A)
    ->  B = A
    ;   absolute_file_name(A, C),
        prolog_to_os_filename(D, C),
        atom_codes(D, E),
        subst([[[0x20], [0'%, 0'2, 0'0]]], E, F),
        atom_codes(G, F),
        atomic_list_concat(['file://', G], B)
    ).

wcacher(A, B) :-
    wcache(A, B),
    !.
wcacher(A, B) :-
    wcache(C, D),
    sub_atom(A, 0, I, _, C),
    sub_atom(A, I, _, 0, E),
    atomic_list_concat([D, E], B).

timestamp(Stamp) :-
    get_time(StampN),
    datetime(StampN, StampC),
    atom_codes(StampA, StampC),
    (   sub_atom(StampA, I, 1, 0, 'Z'),
        I > 23
    ->  sub_atom(StampA, 0, 23, _, StampB),
        atomic_list_concat([StampB, 'Z'], Stamp)
    ;   Stamp = StampA
    ).

uuid(UUID) :-
    Version = 4,
    A is random(0xffffffff),
    B is random(0xffff),
    C is random(0x0fff) \/ Version<<12,
    D is random(0x3fff) \/ 0x8000,
    E is random(0xffffffffffff),
    format(atom(UUID),
           '~`0t~16r~8+-~|\c
            ~`0t~16r~4+-~|\c
            ~`0t~16r~4+-~|\c
            ~`0t~16r~4+-~|\c
            ~`0t~16r~12+', [A, B, C, D, E]).

regex(Pattern, String, List) :-
    escape_atom(Pattern, Pat),
    escape_atom(String, Str),
    re_matchsub(Pat, Str, Dict, []),
    findall(Value,
        (   get_dict(Key, Dict, Value),
            Key \== 0
        ),
        List
    ).

fm(A) :-
    (   nonvar(A),
        A = !
    ->  true
    ;   format(user_error, '*** ~q~n', [A]),
        flush_output(user_error)
    ),
    cnt(fm).

mf(A) :-
    forall(
        catch(A, _, fail),
        (   portray_clause(user_error, A),
            cnt(mf)
        )
    ),
    flush_output(user_error).
