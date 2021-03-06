#!/bin/ksh

set -eu

# Dependencies

PROTOBUF_VERSION="3.0.0-beta-2"
PROTOBUF_JS_VERSION="3.0.0-alpha-5"

# Other settings

PREFIX="ElmProto"

temp_dir="./temp"
definition_directory="./definitions"
protoc_js_out_dir="$temp_dir/protoc-out"

protoc_zip="$temp_dir/protoc.zip"
protoc_dir="$temp_dir/protoc"
protoc_exe="$protoc_dir/protoc"

protobuf_js_zip="$temp_dir/protobuf-js.zip"
protobuf_js_dir="$temp_dir/protobuf-js"
protobuf_js_src_include_dir="$protobuf_js_dir/protobuf-${PROTOBUF_JS_VERSION}/js"
protobuf_js_messagefile="$protobuf_js_src_include_dir/message.js"

closure_library_dir="$temp_dir/closure-library"
closure_library_bin_dir="$closure_library_dir/closure/bin/build"
closure_library_include_dir="$closure_library_dir/closure"
closure_library_third_party_include_dir="$closure_library_dir/third_party/closure"

closure_compiler_zip="$temp_dir/closure-compiler.zip"
closure_compiler_dir="$temp_dir/closure-compiler"
closure_compiler_jar="$closure_compiler_dir/compiler.jar"

closurebuilder_script="$closure_library_bin_dir/closurebuilder.py"
depswriter_script="$closure_library_bin_dir/depswriter.py"

# "Entry point"

echo "Getting Libraries"

if ! [ -d $temp_dir ]
then
  mkdir -p $temp_dir
fi

if [ -f $protoc_exe ]
then
  echo "Protocol buffers compiler binary is already available"
else
  echo "Downloading protocol buffers compiler from github"
  #wget "https://github.com/google/protobuf/releases/download/v3.0.0-beta-2/protoc-${PROTOBUF_VERSION}-win32.zip" -O $protoc_zip
  wget "https://github.com/google/protobuf/releases/download/v3.0.0-beta-2/protoc-${PROTOBUF_VERSION}-osx-x86_64.zip" -O $protoc_zip
  unzip $protoc_zip -d $protoc_dir
fi

if [ -f $protobuf_js_messagefile ]
then
  echo "Protocol buffers js runtime is already installed"
else
  echo "Downloading protocol buffers js runtime"
  wget "https://github.com/google/protobuf/releases/download/v3.0.0-beta-2/protobuf-js-${PROTOBUF_JS_VERSION}.zip" -O $protobuf_js_zip
  unzip $protobuf_js_zip -d $protobuf_js_dir
fi

protobuf_js_binaryfiles=$(find "$protobuf_js_src_include_dir/binary" | grep '.js$' | grep -v '_test.js$')

if [ -d $closure_library_include_dir ]
then
  echo "Closure library is already installed"
else
  echo "Downloading closure library"
  git clone https://github.com/google/closure-library $closure_library_dir
fi

if [ -f $closure_compiler_jar ]
then
  echo "Closure compiler is already installed"
else
  echo "Download closure compiler"
  wget "http://dl.google.com/closure-compiler/compiler-latest.zip" -O $closure_compiler_zip
  unzip $closure_compiler_zip -d $closure_compiler_dir
fi

rm -rf $protoc_js_out_dir
if ! [ -d $protoc_js_out_dir ]
then
  mkdir -p $protoc_js_out_dir
fi

proto_files=$(find $definition_directory | grep .proto$)

# PROTOC

echo "Generating javascript modules from protobuf definitions"
$protoc_exe $proto_files --proto_path $definition_directory --js_out="binary,library=$PREFIX:$protoc_js_out_dir"

# CLOSURE

echo "Combining generated javascript"
# Copy "message and binary/*" into an isolated directory
protobuf_js_include_dir="$temp_dir/protobuf-js-include"
if ! [ -d $protobuf_js_include_dir ]
then
  mkdir -p $protobuf_js_include_dir
fi
cp $protobuf_js_messagefile $protobuf_js_include_dir
for f in $protobuf_js_binaryfiles 
do
  cp $f $protobuf_js_include_dir
done

# Get namsepaces from all generated js files
protoc_js_files=$(find $protoc_js_out_dir | grep .js$)
deps_file="$protoc_js_out_dir/deps.js"
echo $depswriter_script
python $depswriter_script --output_file=$deps_file --root=$protoc_js_out_dir #--root=$protobuf_js_include_dir

output_file=contracts/Native/Proto.js
proto_modulename="Native.$PREFIX"

# Stitch JavaScript
echo $closurebuilder_script
python $closurebuilder_script --output_file="$output_file" --output_mode=compiled --compiler_jar="$closure_compiler_jar" --root=$protoc_js_out_dir --root=$protobuf_js_include_dir --root=$closure_library_include_dir --root=$closure_library_third_party_include_dir --input=$deps_file --namespace="proto.world.GameUpdate" 

# Wrapper
cat >>$output_file <<EOF
Elm.${proto_modulename} = Elm.${proto_modulename} || {};
Elm.${proto_modulename}.make = function(_elm) {
  "use strict";
  _elm.${proto_modulename} = _elm.${proto_modulename} || {};
  if (_elm.${proto_modulename}.values) {
    return _elm.${proto_modulename}.values;
  }
  return _elm.${proto_modulename}.values = $PREFIX;
}
EOF

# Compile Elm - NOTE DO NOT INCLUDE IN elm-protoc EVENTUAL 
echo "Compiling Main"
elm make ./src/Main.elm 
