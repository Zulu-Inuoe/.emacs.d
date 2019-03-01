$MemberDef = @"
[DllImport("gdi32.dll")]
public static extern int AddFontResource(string lpszFilename);
"@,
@"
[DllImport("gdi32.dll")]
public static extern bool RemoveFontResource(string lpszFilename);
"@,
@"
[DllImport("user32.dll", CharSet = CharSet.Auto)]
public static extern IntPtr SendMessage(IntPtr hWnd, int Msg, int wParam, IntPtr lParam);
"@

$WinAPI = Add-Type -name WinAPI -MemberDefinition $MemberDef -PassThru
