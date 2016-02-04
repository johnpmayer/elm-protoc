{-# LANGUAGE CPP #-}

module Constants where

import System.IO.Unsafe
import System.Directory
import System.FilePath

-- Shared temporary directory
-- TODO consider changing this to something like elm-stuff/elm-protoc-temp

temp_dir :: FilePath
temp_dir = unsafePerformIO getCurrentDirectory </> "elm-stuff" </> "elm-protoc-temp"

temp_js_out_dir :: FilePath
temp_js_out_dir = temp_dir </> "js_out"

js_deps_file :: FilePath
js_deps_file = temp_js_out_dir </> "deps.js"

native_js_out_filename :: FilePath
native_js_out_filename = "Proto.js"

-- Protocol Buffers Compiler

protoc_version :: String
protoc_version = "3.0.0-beta-2"

protoc_url :: String
#ifdef mingw32_HOST_OS
protoc_url = "https://github.com/google/protobuf/releases/download/v" ++ protoc_version ++ "/protoc-" ++ protoc_version ++ "-win32.zip"
#endif
#ifdef darwin_HOST_OS
protoc_url = "https://github.com/google/protobuf/releases/download/v" ++ protoc_version ++ "/protoc-" ++ protoc_version ++ "-osx-x86_64.zip"
#endif

protoc_dir :: FilePath
protoc_dir = temp_dir </> "protoc"

protoc_exe :: FilePath
protoc_exe = protoc_dir </> "protoc.exe"

-- Protocol Buffers JS Includes

protobuf_js_version :: String
protobuf_js_version = "3.0.0-alpha-5"

protobuf_js_url :: String
protobuf_js_url = "https://github.com/google/protobuf/releases/download/v" ++ protoc_version ++ "/protobuf-js-" ++ protobuf_js_version ++ ".zip"

protobuf_js_dir :: FilePath
protobuf_js_dir = temp_dir </> "protobuf_js"

protobuf_js_include_dir :: FilePath
protobuf_js_include_dir = protobuf_js_dir </> ("protobuf-" ++ protobuf_js_version) </> "js"

protobuf_js_binary_include_dir :: FilePath
protobuf_js_binary_include_dir = protobuf_js_include_dir </> "binary"

-- do we need the message file explicitly?
-- protobuf_js_messagefile
-- protobuf_js_binaryfiles

closure_library_repository :: String
closure_library_repository = "https://github.com/google/closure-library"

closure_library_dir :: FilePath
closure_library_dir = temp_dir </> "closure_library"

closure_library_bin_dir :: FilePath
closure_library_bin_dir = closure_library_dir </> "closure" </> "bin" </> "build"

closurebuilder_script :: FilePath
closurebuilder_script = closure_library_bin_dir </> "closurebuilder.py"

depswriter_script :: FilePath
depswriter_script = closure_library_bin_dir </> "depswriter.py"

closure_library_include_dir :: FilePath
closure_library_include_dir = closure_library_dir </> "closure"

closure_library_third_party_include_dir :: FilePath
closure_library_third_party_include_dir = closure_library_dir </> "third_party" </> "closure"
