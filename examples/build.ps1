
# Dependencies

$PROTOBUF_VERSION = "3.0.0-beta-2"
$PROTOBUF_JS_VERSION = "3.0.0-alpha-5"

# Other settings

$temp_dir = ".\temp"

$protoc_zip = Join-Path $temp_dir "protoc.zip"
$protoc_dir = ".\protoc"
$protoc_exe = Join-Path $protoc_dir "protoc.exe"

$protobuf_js_zip = Join-Path $temp_dir "protobuf-js.zip"
$protobuf_js_dir = ".\protobuf-js"
$protobuf_js_messagefile = [io.path]::Combine($protobuf_js_dir, "protobuf-${PROTOBUF_JS_VERSION}", "js", "message.js")

$closure_library_dir = ".\closure-library"
$closure_library_bin_dir = [io.path]::Combine($closure_library_dir, "closure\bin\build")
$closure_library_include_dir  = [io.path]::Combine($closure_library_dir, "closure")
$closure_library_third_party_include_dir  = [io.path]::Combine($closure_library_dir, "third_party", "closure")

$closurebuilder_script = Join-Path $closure_library_bin_dir "closurebuilder.py"
$depswriter_script = Join-Path $closure_library_bin_dir "depswriter.py"

<#
$closure_compiler_zip = Join-Path $temp_dir "closure-compiler.zip"
$closure_compiler_dir = ".\closure-compiler"
$closure_compiler_jar = Join-Path $closure_compiler_dir "compiler.jar"
#>

# "Entry point"

$ErrorActionPreference = "STOP"

Write-Host "Getting Libraries"

if (-not (Test-Path $temp_dir)) { New-Item -ItemType Directory -Path $temp_dir }

if (& $protoc_exe --version) {
    Write-Host "Protocol buffers compiler binary is already available"
}
else {
    Write-Host "Downloading protocol buffers compiler from github"
    Invoke-WebRequest "https://github.com/google/protobuf/releases/download/v3.0.0-beta-2/protoc-${PROTOBUF_VERSION}-win32.zip" -OutFile $protoc_zip
    Expand-Archive $protoc_zip $protoc_dir
}

if (Test-Path $protobuf_js_messagefile) {
    Write-Host "Protocol buffers js runtime is already installed"
}
else {
    Write-Host "Downloading protocol buffers js runtime"
    Invoke-WebRequest "https://github.com/google/protobuf/releases/download/v3.0.0-beta-2/protobuf-js-${PROTOBUF_JS_VERSION}.zip" -OutFile $protobuf_js_zip
    Expand-Archive $protobuf_js_zip $protobuf_js_dir
}

if (Test-Path $closure_library_include_dir) {
    Write-Host "Closure library is already installed"
}
else {
    Write-Host "Downloading closure library"
    & git clone https://github.com/google/closure-library $closure_library_dir
}

<#
if (Test-Path $closure_compiler_jar) {
    Write-Host "Closure compiler is already installed"
}
else {
    Write-Host "Downloading closure compiler"
    Invoke-WebRequest "http://dl.google.com/closure-compiler/compiler-latest.zip" -OutFile $closure_compiler_zip
    Unzip-Archive $closure_compiler_zip $closure_compiler_dir
}
#>

$definition_directory = ".\definitions"
$protoc_js_out_dir = Join-Path $temp_dir "protoc-out"
if (-not (Test-Path $protoc_js_out_dir)) { New-Item -ItemType Directory -Path $protoc_js_out_dir }

$proto_files = Get-ChildItem $definition_directory | %{ Join-Path $definition_directory $_ }

Write-Host "Generating javascript modules from protobuf definitions"
& protoc $proto_files --proto_path $definition_directory --js_out $protoc_js_out_dir

Write-Host "Combining generated javascript"
# Copy "Message" into an isolated directory
$protobuf_js_include_dir = Join-Path $temp_dir "protobuf-js"
if (-not (Test-Path $protobuf_js_include_dir)) { New-Item -ItemType Directory -Path $protobuf_js_include_dir }
Copy-Item $protobuf_js_messagefile $protobuf_js_include_dir
# Get namsepaces from all generated js files
$protoc_js_files = Get-ChildItem $protoc_js_out_dir | %{ Join-Path $protoc_js_out_dir $_ }
$protoc_js_include_flags = Get-ChildItem $protoc_js_out_dir | %{ Join-Path $protoc_js_out_dir $_ } | %{ "--input=$_" }
& python $depswriter_script --output_file=deps.js --root=$protoc_js_out_dir
& python $closurebuilder_script --help --output_file=out.js --output_mode=script --root=$protoc_js_out_dir --root=$protobuf_js_include_dir --root=$closure_library_include_dir --root=$closure_library_third_party_include_dir $protoc_js_include_flags
# & java -jar $closure_compiler_jar --js_output_file=out.js "${protoc_js_out_dir}\**.js" "${protobuf_js_include_dir}\**.js" "${closure_library_include_dir}\**.js" "${closure_library_third_party_include_dir}\**.js" '!**_test.js'

Write-Host "Compiling Main"
& elm make .\src\Main.elm --output elm.js