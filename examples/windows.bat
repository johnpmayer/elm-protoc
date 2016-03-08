stack build
stack exec elm-protoc -- --input-directory definitions --output-directory contracts --module-prefix ExampleProto
elm make