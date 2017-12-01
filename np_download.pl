:- module(np_download,
          [ np_download/1
          ]).
:- use_module(library(http/http_open)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/turtle)).

np_server('http://np.inn.ac/').

hash('RAvJLzqaDFPAVwd9qbwSjXFlZMVuFZBvm6qlGUqZUCPY0').


np_download(Hash) :-
    np_server(Server),
    atom_concat(Server, Hash, URL),
    rdf_load(URL, [format(trig)]).
