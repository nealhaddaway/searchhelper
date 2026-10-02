# Search Helper

A Shiny application for developing systematic-map search strategies using benchmark records, citation chasing and candidate-term discovery.

## Planned workflow

The application has two entry routes:

1. **Benchmark route** — upload known relevant records as RIS, provide a draft Boolean search, resolve the benchmarks in Lens, retrieve backward references and forward citations, identify records missed by the search, and mine them for candidate search terms.
2. **Concept route** — create any number of search concept blocks, retrieve a relevance-ranked sample from Lens, screen records as relevant/not relevant, and promote included records to benchmarks before running the same citation-chasing and term-discovery workflow.

Concept blocks are optional and user-defined. Suggested labels include Population, Intervention or exposure, Outcome, Study design and Context, but a concept may be represented by multiple Boolean substrings when scientifically appropriate.

## Stage 1

The current development branch implements the first benchmark proof of concept:

- RIS upload and parsing
- DOI → PMID → exact-title fallback resolution in Lens
- backward reference retrieval
- forward citation retrieval
- citation-record metadata retrieval
- CSV export
- no writes to external services

The Lens token is read from the environment variable `LENS_API_TOKEN`. Never commit the token.

## Search-language principles

The internal representation will use conventional Boolean logic rather than database-specific syntax. Later stages will provide explicit, inspectable suggestions about:

- synonyms and candidate terms
- wildcard/truncation stems
- phrases
- AND combinations
- proximity formulations
- alternative concept-block structures

Suggestions will not silently alter the user's search string.

## Deployment

Target: Posit-hosted Shiny deployment.


## General lexical expansion

Optional vocabulary expansion uses the Datamuse API to suggest morphological variants and general English synonyms for terms already present in the search. Synonym relations are backed by WordNet. These suggestions are kept separate from corpus evidence and are never added to the search automatically.

Datamuse API: https://www.datamuse.com/api/
