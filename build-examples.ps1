[CmdletBinding()]
param(
  [ValidateSet('Default', 'Release')]
  [string] $BuildMode = 'Release',

  [switch] $Rebuild
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

$RepoRoot = $PSScriptRoot
$ExamplesDirectory = Join-Path $RepoRoot 'examples'
$OutputDirectory = Join-Path $RepoRoot 'example-bin'
$LazBuild = Get-Command lazbuild -CommandType Application -ErrorAction Stop
$Projects = @(
  Get-ChildItem -LiteralPath $ExamplesDirectory -Directory |
    ForEach-Object {
      Get-ChildItem -LiteralPath $_.FullName -Filter '*.lpi' -File
    } |
    Sort-Object FullName
)

if ($Projects.Count -eq 0) {
  throw "No Lazarus example projects found under '$ExamplesDirectory'."
}

$null = New-Item -ItemType Directory -Path $OutputDirectory -Force
$BuildArguments = @("--build-mode=$BuildMode", '--no-write-project')
if ($Rebuild) {
  $BuildArguments += '--build-all'
}

foreach ($Project in $Projects) {
  $RelativeProject = $Project.FullName.Substring($RepoRoot.Length + 1)
  Write-Host "==> Building $RelativeProject ($BuildMode)"
  & $LazBuild.Source @BuildArguments $Project.FullName
  if ($LASTEXITCODE -ne 0) {
    throw "lazbuild failed for '$RelativeProject' with exit code $LASTEXITCODE."
  }
}

Write-Host "Built $($Projects.Count) examples into '$OutputDirectory'."
