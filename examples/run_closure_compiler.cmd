cp ExampleProtoDeps.js elm-stuff/elm-protoc-temp/js_out/ExampleProtoDeps.js
java -jar elm-stuff/elm-protoc-temp/closure_compiler/compiler.jar --dependency_mode STRICT --entry_point goog:ExampleProto elm-stuff/elm-protoc-temp/js_out/ elm-stuff/elm-protoc-temp/protobuf_js_include/ elm-stuff/elm-protoc-temp/closure_library/closure/
