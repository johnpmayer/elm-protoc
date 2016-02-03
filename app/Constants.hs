{-# LANGUAGE CPP #-}

module Constants where

import System.IO.Unsafe
import System.FilePath
import System.Directory

-- Shared temporary directory
-- TODO consider changing this to something like elm-stuff/elm-protoc-temp

tempDir :: FilePath
tempDir = unsafePerformIO getCurrentDirectory </> "elm-stuff" </> "elm-protoc-temp"

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
protoc_dir = tempDir </> "protoc"

protoc_exe :: FilePath
protoc_exe = protoc_dir </> "protoc.exe"

-- Protocol Buffers JS Includes

protobuf_js_version :: String
protobuf_js_version = "3.0.0-alpha-5"

protobuf_js_url :: String
protobuf_js_url = "https://github.com/google/protobuf/releases/download/v" ++ protoc_version ++ "/protobuf-js-" ++ protobuf_js_version ++ ".zip"

protobuf_js_dir :: FilePath
protobuf_js_dir = tempDir </> "protobuf_js"

protobuf_js_include_dir :: FilePath
protobuf_js_include_dir = protobuf_js_dir </> ("protobuf-" ++ protobuf_js_version) </> "js"

-- do we need the message file explicitly?
-- protobuf_js_messagefile
-- protobuf_js_binaryfiles

closure_library_dir :: FilePath
closure_library_dir = tempDir </> "closure_library"

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
