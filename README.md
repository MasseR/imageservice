# Service for detecting duplicate images

[![Build Status](https://travis-ci.org/MasseR/imageservice.svg?branch=master)](https://travis-ci.org/MasseR/imageservice)

*Disclaimer*: This is just for fun.

A twineye like service for finding duplicate images.

The service scans subreddits, going through the images in there, computing a
fingerprint for each image and adding them to an index.

I will probably add api docs at some point, but for now check `src/API.hs` to
see what kinds of endpoints are available.

## Fingerprint

Two fingerprinting algorithms in the codebase, average hash and dhash
([explanation here](https://jenssegers.com/61/perceptual-image-hashes)).

Mostly because this is for fun, I have abstracted the algorithms as a Store
comonad.

## Index

Indexing is done through BK-trees.
