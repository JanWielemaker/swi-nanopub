:- module(np_download,
          [ np_download/1,
            np_triples/2,      % Hash, Triples
            np_graphs/5        % +Triples, NPURI, Assertions, Prov, Pub
          ]).
:- use_module(library(http/http_open)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/turtle)).

:- rdf_register_prefix(np, 'http://www.nanopub.org/nschema#').

np_server('http://np.inn.ac/').

hash('RAvJLzqaDFPAVwd9qbwSjXFlZMVuFZBvm6qlGUqZUCPY0').


np_download(Hash) :-
    np_server(Server),
    atom_concat(Server, Hash, URL),
    rdf_load(URL, [format(trig)]).

%!  np_triples(+Hash, -Triples)

np_triples(Hash, Triples) :-
    np_server(Server),
    atom_concat(Server, Hash, URL),
    setup_call_cleanup(
        (   http_open(URL, In, []),
            set_stream(In, encoding(utf8))
        ),
        rdf_read_turtle(In, Triples, []),
        close(In)).

np_graphs(Triples, NPURI,
          AssertionGraph, ProvGraph, PubGraph) :-
    rdf_equal(np:hasAssertion, PA),
    rdf_equal(np:hasProvenance, PPo),
    rdf_equal(np:hasPublicationInfo, PPi),
    memberchk(rdf(NPURI, PA,  AssertionGraph, HeadGraph:_), Triples),
    memberchk(rdf(NPURI, PPo, ProvGraph, HeadGraph:_), Triples),
    memberchk(rdf(NPURI, PPi,  PubGraph, HeadGraph:_), Triples).
