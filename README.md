# elm-protoc

Elm utility for generating Elm &amp; Native JavaScript source for Google Protocol Buffers

First things first, if you're just interested in generating pure Elm JSON decoders/encoders from protocol buffer message definitions, shout out to https://github.com/tiziano88/elm-protobuf which is probably more appropriate for most projects

## Architecture

elm-protoc is NOT a protoc plugin, but is rather a standalone app that uses protoc to produce JavaScript, and then generates Elm Types & to match your protobuf messages, and finally generates Elm functions (& Native modules bindings) for serializing the Elm value to/from the binary wire format.
