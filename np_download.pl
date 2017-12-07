:- module(np_download,
          [ load_nanopub/1,              % +NanopubID
            download_nanopub_triples/2,  % +ArtifactCode, -Triples
            np_graphs/5,                 % +Triples, NPURI, Assertions, Prov, Pub
			get_nanopub/2                % +NanopubID, -Nanopub
          ]).
:- use_module(library(http/http_open)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/turtle)).

:- rdf_register_prefix(np, 'http://www.nanopub.org/nschema#').


np_server('http://np.inn.ac/').
np_server('http://app.petapico.d2s.labs.vu.nl/nanopub-server/').
np_server('http://openphacts.cs.man.ac.uk:8080/nanopub-server/').
np_server('http://nanopubs.semanticscience.org/').
np_server('https://nanopubs.dumontierlab.com/').
np_server('http://akswnc9.aksw.uni-leipzig.de:8110/nanopub-server/').
np_server('http://nanopub.backend1.scify.org/nanopub-server/').
np_server('http://nanopubs.restdesc.org/').
np_server('http://nanopubs.stanford.edu/nanopub-server/').
np_server('http://rdf.disgenet.org/nanopub-server/').


load_nanopub(NanopubID) :-
	get_artifactcode(NanopubID, ArtifactCode),
    np_server(Server),
    atom_concat(Server, ArtifactCode, URL),
    rdf_load(URL, [format(trig)]).

%!  np_triples(+Hash, -Triples)

download_nanopub_triples(ArtifactCode, Triples) :-
    np_server(Server),
    atom_concat(Server, ArtifactCode, URL),
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


get_nanopub(NanopubID, Nanopub) :-
	get_artifactcode(NanopubID, ArtifactCode),
	download_nanopub_triples(ArtifactCode, Nanopub).

get_artifactcode(NanopubID, ArtifactCode) :-
	(
		trustyuri_artifactcode(NanopubID, ArtifactCode)
	;
		artifactcode(NanopubID, 'RA'),
		ArtifactCode = NanopubID
	),
	!.

trustyuri_artifactcode(TrustyUri, ArtifactCode) :-
	sub_atom(TrustyUri, _, _, _, '://'),
	sub_atom(TrustyUri, _, 45, 0, ArtifactCode).

artifactcode(ArtifactCode, Type) :-
	atom_length(ArtifactCode, 45),
	sub_atom(ArtifactCode, 0, 2, _, Type).

