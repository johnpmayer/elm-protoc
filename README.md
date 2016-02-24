# elm-protoc

Elm utility for generating Elm &amp; Native JavaScript source for Google Protocol Buffers

## Architecture

elm-protoc is NOT a protoc plugin, but is rather a standalone app that uses protoc to produce JavaScript, and then generates Elm Types & to match your protobuf messages, and finally generates Elm functions (& Native modules bindings) for serializing the Elm value to/from the binary wire format.
