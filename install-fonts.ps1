. "$PSScriptRoot\winapi.ps1"

Get-Childitem "${PSScriptRoot}\fonts\" -Filter "*.ttf" |
  Foreach-Object {
      $WinAPI::AddFontResource($_.FullName)
  } | Out-Null

$WinAPI::SendMessage(-3, 0x001D, 0, 0) | Out-Null
