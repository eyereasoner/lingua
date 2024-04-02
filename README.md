# The lingua reasoner

Reasoning engine that is using RDF TriG as the web lingua.

Examples and test cases are in [tests](https://github.com/eyereasoner/lingua/tree/main/tests) and their output in [tests/output](https://github.com/eyereasoner/lingua/tree/main/tests/output)

```
Usage: lingua <options>* <data>*

<options>
    --genid <genid>             use <genid> in Skolem IRIs
    --help                      show help info
    --output <file>             write reasoner output to <file>
    --version                   show version info
    --wcache <uri> <file>       to tell that <uri> is cached as <file>

<data>
    <uri>                       TriG data
```

## RDF Lingua

RDF TriG as the web lingua.

Lingua supports reasoning with forward rules described in RDF as
```
_:ng1 lingua:implication _:ng2.

_:ng1 {
    RDF triples
}

_:ng2 {
    RDF triples
}
```

A forward rule with `lingua:implication false` is an inference fuse.

Lingua also supports reasoning with backward rules described in RDF as
```
_:ng1 lingua:hornb _:ng2.   # hornb is acronym for "head or not body"

_:ng1 {
    RDF triples
}

_:ng2 {
    RDF triples
}
```

Lingua also supports querying with queries described in RDF as
```
_:ng1 lingua:query _:ng2.

_:ng1 {
    RDF triples
}

_:ng2 {
    RDF triples
}
```

The `lingua:` prefix is `<http://www.w3.org/2000/10/swap/lingua#>` and is rooted
in the "Semantic Web Area for Play" `http://www.w3.org/2000/10/swap/` URI.

The `var:` prefix is `<http://www.w3.org/2000/10/swap/var#>` and is used for
variables that are interpreted as universally quantified variables except for
forward rule conclusion-only variables which are interpreted existentially.

Literal subjects are described as
```
[] rdf:value "aha"; :p :o.
```
