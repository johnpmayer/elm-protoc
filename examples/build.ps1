
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
$protobuf_js_includedir = [io.path]::Combine($protobuf_js_dir, "protobuf-${PROTOBUF_JS_VERSION}", "js")

$closure_dir = ".\closure-library"
$closure_bin_dir = [io.path]::Combine($closure_dir, "closure\bin\build")

$closurebuilder_script = Join-Path $closure_bin_dir "closurebuilder.py"
$depswriter_script = Join-Path $closure_bin_dir "depswriter.py"

# Utilities

Add-Type -AssemblyName System.IO.Compression.FileSystem
function Unzip-Archive
{
    param([string]$zipfile, [string]$outpath)

    [System.IO.Compression.ZipFile]::ExtractToDirectory($zipfile, $outpath)
}

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
    Unzip-Archive $protoc_zip $protoc_dir
}

if (Test-Path (Join-Path $protobuf_js_includedir "message.js")) {
    Write-Host "Protocol buffers js runtime is already installed"
}
else {
    Write-Host "Downloading protocol buffers js runtime"
    Invoke-WebRequest "https://github.com/google/protobuf/releases/download/v3.0.0-beta-2/protobuf-js-${PROTOBUF_JS_VERSION}.zip" -OutFile $protobuf_js_zip
    Unzip-Archive $protobuf_js_zip $protobuf_js_dir
}

if (Test-Path $closurebuilder_script) {
    Write-Host "Closure library is already installed"
}
else {
    Write-Host "Downloading closure library"
    & git clone https://github.com/google/closure-library $closure_dir
}

$definition_directory = ".\definitions"
$protoc_js_out = Join-Path $temp_dir "protoc_out"
if (-not (Test-Path $protoc_js_out)) { New-Item -ItemType Directory -Path $protoc_js_out }

Write-Host "Generating javascript modules from protobuf definitions"
& protoc $(Get-ChildItem $definition_directory | %{ Join-Path $definition_directory $_ }) --proto_path $definition_directory --js_out $protoc_js_out

Write-Host "Combining generated javascript"
& python $closurebuilder_script --root $protobuf_js_includedir --root $protoc_js_out

Write-Host "Compiling Main"
& elm make .\src\Main.elm --output elm.js