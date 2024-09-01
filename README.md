# The lingua reasoner

[![DOI](https://zenodo.org/badge/781099490.svg)](https://zenodo.org/doi/10.5281/zenodo.13370910)

Reasoning engine that is using RDF TriG as the web lingua.

See [examples and cases](https://github.com/eyereasoner/lingua/tree/main/cases) and their [output](https://github.com/eyereasoner/lingua/tree/main/cases/output)

```
Usage: lingua <options>* <data>*

<options>
    --genid <genid>             use <genid> in Skolem IRIs
    --explain                   explain the reasoning steps
    --help, -h                  show help info
    --output <file>             write reasoner output to <file>
    --version, -v               show version info
    --wcache <uri> <file>       to tell that <uri> is cached as <file>

<data>
    <uri>                       TriG data
```

## Install and test

```bash
git clone https://github.com/eyereasoner/lingua
cd lingua
./install
./test
```

## RDF lingua

RDF TriG as the web lingua using blank node graphs.

Lingua supports reasoning with forward rules described in RDF as
```
_:bng_1 log:implies _:bng_2.

_:bng_1 {
    RDF triples
}

_:bng_2 {
    RDF triples
}
```

A forward rule with `log:implies false` is an inference fuse.

Lingua also supports reasoning with backward rules described in RDF as
```
_:bng_1 log:isImpliedBy _:bng_2.

_:bng_1 {
    RDF triple
}

_:bng_2 {
    RDF triples
}
```

Lingua also supports querying with queries described in RDF as
```
_:bng_1 log:query _:bng_2.

_:bng_1 {
    RDF triples
}

_:bng_2 {
    RDF triples
}
```

The `var:` prefix is `<http://www.w3.org/2000/10/swap/var#>` and is used for
variables that are interpreted as universally quantified variables except for
forward rule conclusion-only variables which are interpreted existentially.

Literal subjects are described as
```
[] rdf:value "aha"; :p :o.
```
