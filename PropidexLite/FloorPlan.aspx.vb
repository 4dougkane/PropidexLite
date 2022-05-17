Imports PropidexLite.SQLServer
Imports System.Drawing
Imports System.IO
Imports System.Collections
Imports System.Collections.Generic
Imports System.Web
Imports System.Web.UI.HtmlControls
Imports System.Threading
Imports System.Security.Principal

Public Class FloorPlan
    Inherits System.Web.UI.Page

    Private fx1, fx2, fy1, fy2 As Single 'minmax
    Private sx, sy As Single ' scale transform
    Private ox, oy As Single ' translate transform
    Private lx1, lx2, ly1, ly2 As Single
    Private px1, py1, px2, py2 As Single
    Private bg() As PXF
    Private lg() As PXF
    Private fg() As MapType
    Private rg() As SPIN
    Private circuit() As PXF
    Private zoo() As ZPAIR
    Private ScaleFactor As Integer
    Private FontSize1 As Single = 24
    Private FontSize2 As Single = 24
    Private FontSize3 As Single = 24
    Private FontSize4 As Single = 24
    Private FontSize5 As Single = 24
    Private FontSize6 As Single = 24
    Private FontSize7 As Single = 24
    Private DXFScale As Single = 1
    Public Shared IDLEN As Integer = 16
    Private Market() As MarkerLabel

    Private DrawingName As String
    Private FloorRentalFactor As Single
    Private NetArea(15) As Single
    Private YearEnd As Integer = 0
    Private MallType As Integer
    Private NOF As Integer
    Private BOMA As Integer
    Private Calculated As Single = 0
    Private CalculatedFactor As Single
    Private FloorRentable As Single = 0
    Private METRIC As Boolean = False
    Private detailsList As List(Of detailsLists)
    Private SC(300) As Integer
    Private viewlist As List(Of viewLists) = New List(Of viewLists)
    Private AvailTypes As New Dictionary(Of String, Integer)
    Private ViewColor(9) As Integer
    Private VSET As String
    Private ViewNumber As String
    Private Propertynumber As String
    Private FNo As String
    Private UserID As String = ""
    Public Shared PER As String = "PSF"
    Private labelsize As Single
    Private ActionArray(1) As ActionObject
    Private actionmode As Integer
    Private actiontype As Integer
    Public Shared TypeFilter As String = ""
    Private MapNumber As Integer = 0
    Public Shared OQN(7) As Integer
    Public Shared VQN(7) As Integer
    Public Shared OLN(3) As String
    Public Shared VLN(3) As String
    Private OL() As Integer
    Private VL() As Integer
    Private oSuffix(3) As String
    Private vSuffix(3) As String
    Private ViewType As String = ""
    Private Customary As Integer = 511
    Private Symbol(16) As Color
    Private DimensionFormat As Integer
    Public Shared piX2 As Single = Math.Atan(1) * 8
    Private Highlight As Integer = -1
    Private Drawlabel As String
    Private SysData As DataTable

    Private Sub FloorPlan_AbortTransaction(sender As Object, e As System.EventArgs) Handles Me.AbortTransaction

    End Sub


    Protected Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        Session("QNO") = ""
        Session("QNO1") = ""
        Session("QNO2") = ""
        Session("Connection") = ""
        If Request.QueryString.Count > 0 Then
            Session("Connection") = SQLServer.getConnection
            Dim Server As String = My.Settings.Server
            Dim DatabaseName As String = My.Settings.DatabaseName
            Dim DatabaseUID As String = My.Settings.DatabaseUID
            Dim DatabasePWD As String = My.Settings.DatabasePWD
            If Request.QueryString("pno") IsNot Nothing AndAlso Request.QueryString("pno").Trim().Length > 0 Then
                Propertynumber = Request.QueryString("pno")
                If Request.QueryString("fno") IsNot Nothing AndAlso Request.QueryString("fno").Trim().Length > 0 Then
                    FNo = Request.QueryString("fno")
                    If Request.QueryString("vno") IsNot Nothing AndAlso Request.QueryString("vno").Trim().Length > 0 Then
                        ViewNumber = Request.QueryString("vno")
                        'get viewcolor
                        Dim PQ As DataTable = GetTable(Session("Connection"), "SELECT VSET, BOMA, TabPak, PName,YearEnd,Highlight FROM Properties WHERE PNo='" & Propertynumber & "'")
                        If PQ.Rows.Count > 0 Then
                            Dim PS As DataRow = PQ.Rows(0)
                            Dim BOMA As Integer = 0
                            If Not IsDBNull(PS("BOMA")) Then BOMA = PS("BOMA")
                            If Not IsDBNull(PS("Highlight")) Then Highlight = PS("Highlight")
                            BOMA = BOMA And 7
                            Dim TabPak As String = PS("TabPak") & ""
                            VSET = PS("VSET") & ""
                            YearEnd = PS("YearEnd")
                            Dim Culture = UCase(System.Globalization.CultureInfo.CurrentCulture.Name)
                            TSQL = "SELECT * FROM Cultures"
                            Dim DT As DataTable = GetTable(Session("Connection"), TSQL)
                            If DT.Rows.Count Then
                                Session("Lex") = ""
                                For Each DR As DataRow In DT.Rows
                                    Session("Lex") = Trim(DR("LEX") & "")
                                    If UCase(DR("Culture")) = Culture Then Exit For
                                Next
                            Else
                                MsgBox("Login Failed")
                                Exit Sub
                            End If
                            ViewType = GetValue(Session("Connection"), "SELECT VType FROM " & Session("Lex") & "Themes where VNo=" & ViewNumber & "") & ""
                            TSQL = "SELECT Form, Item, Caption FROM " & Session("LEX") & "Resources"
                            LanguageResources = GetTable(Session("Connection"), TSQL)
                            If Len(Trim(ViewNumber)) = 0 Then Exit Sub
                            Dim QTable As DataTable = GetTable(Session("Connection"), "SELECT QNo,QNo1,QNo2 FROM " & Session("Lex") & "Themes WHERE VNo=" & ViewNumber & "")
                            Session("QNo") = QTable(0)("QNo")
                            If Not IsDBNull(QTable(0)("QNo1")) Then Session("QNo1") = QTable(0)("QNo1")
                            If Not IsDBNull(QTable(0)("QNo2")) Then Session("QNo2") = QTable(0)("QNo2")

                            'Find USerID
                            Dim UserName As String = HttpContext.Current.User.Identity.Name.ToString()
                            If Len(Trim((UserName))) = 0 Then UserName = System.Security.Principal.WindowsIdentity.GetCurrent.Name

                            ' Dim id = Thread.CurrentPrincipal.Identity.Name
                            '  Dim UserName As String = System.Security.Principal.WindowsIdentity.GetCurrent.Name
                            'Response.Output.WriteLine(" server agent " & Request.ServerVariables("REMOTE_USER") & " " & Request.ServerVariables("LOGON_USER") & "Host name" & Request.UserHostName & " Thread Id " & id & "  " & "UserIdentity " & HttpContext.Current.User.Identity.Name.ToString() & "   " & "Principal " & System.Security.Principal.WindowsIdentity.GetCurrent().Name.ToString() & " Logon" & Request.LogonUserIdentity.Name)

                            Dim WinID() As String = Split(UserName, "\")
                            UserID = UCase(Trim(WinID(1)))
                            If Len(UserID) = 0 Then Exit Sub
                            TSQL = "SELECT * FROM Security WHERE (UserID='" & UserID & "') AND (PNo='" & Propertynumber & "')"
                            Dim ST As DataTable = GetTable(Session("Connection"), TSQL)
                            Session("UserLevel") = 0
                            If ST.Rows.Count Then
                                Session("UserLevel") = ST.Rows(0)("UserLevel")
                            End If
                            If Len(Trim(GetValue(Session("Connection"), "SELECT VNo FROM " & Session("LEX") & "Themes WHERE (UserLevel<=" & Session("UserLevel") & ") AND (Budget=0) AND (VSET='" & VSET & "' OR VSET IS NULL) AND VNo=" & ViewNumber & ""))) = 0 Then
                                MsgBox("Please check your userlevel")
                                Exit Sub
                            End If


                            TSQL = "SELECT u.*, m.* FROM Users u LEFT JOIN " & Session("Lex") & "UofMs m ON u.UofM=m.UofM WHERE UserID='" & UserID & "'"
                            Dim UT As DataTable = GetTable(Session("Connection"), TSQL)
                            If UT.Rows.Count Then
                                Dim UR As DataRow = UT.Rows(0)
                                If Not IsDBNull(UR("UofM")) Then UofM = UR("UofM")
                                If Not IsDBNull(UR("UofA")) Then UofA = UR("UofA")
                                If Not IsDBNull(UR("UPSF")) Then UPSF = UR("UPSF")
                                Try
                                    If Not IsDBNull(UR("PER")) Then PER = UR("PER")
                                Catch ex As Exception
                                    MsgBox(ex.Message)
                                    PER = ""
                                End Try
                                Try
                                    MyAction = UR("Action") & ""
                                    Dim n As Integer = 0
                                    TSQL = "SELECT * FROM " & Session("LEX") & "Actions WHERE Action='" & UR("Action") & "'"
                                    DT = GetTable(Session("Connection"), TSQL)
                                    ActionArray(n).Name = DT(0)("Action") & ""
                                    ActionArray(n).Mode = DT(0)("Mode")
                                    ActionArray(n).Type = DT(0)("Type")
                                    ActionArray(n).Filter = DT(0)("Filter") & ""
                                Catch ex As Exception
                                    Dim n As Integer = 0
                                    TSQL = "SELECT * FROM " & Session("LEX") & "Actions WHERE (Control=0 OR Control=2) AND (UserLevel<=" & Session("UserLevel") & ") ORDER BY Sequence"
                                    DT = GetTable(Session("Connection"), TSQL)
                                    ReDim ActionArray(DT.Rows.Count - 1)
                                    For Each DR In DT.Rows
                                        ActionArray(n).Name = DR("Action") & ""
                                        ActionArray(n).Mode = DR("Mode")
                                        ActionArray(n).Type = DR("Type")
                                        ActionArray(n).Filter = DR("Filter") & ""
                                        'If IsDBNull(DR("VNo")) Then
                                        '    ActionArray(n).View = -1
                                        'Else
                                        '    ActionArray(n).View = DR("VNo")
                                        'End If
                                        n = n + 1
                                    Next
                                    n = ActionArray.Length - 1
                                    MyAction = ActionArray(n).Name
                                End Try
                            End If
                            If Request.QueryString("mno") IsNot Nothing AndAlso Request.QueryString("mno").Trim().Length > 0 Then MapNumber = Convert.ToInt32(Request.QueryString("mno"))
                            generateFloorPlan()
                        End If
                    Else
                        Exit Sub
                    End If
                End If
            End If
        End If
    End Sub

    Private Sub generateFloorPlan()
        Dim MapType As Integer = 0
        Dim PropertyName As String
        Dim svgX As Integer = 960
        Dim svgY As Integer = 760

        'only Master drawing
        If MapNumber > 0 Then
            TSQL = "SELECT Maps.Name, Properties.PName, Maps.MNo, Maps.DXFScale, Maps.Size1, Maps.Size2, Maps.Size3, Maps.Size4,Maps.Style4, CONVERT(nvarchar(50), Maps.SavedOn, 120) AS NOW, XData.XNo, Floors.PNo, Floors.FNo, Floors.Elevation, Maps.Type FROM Properties INNER JOIN Maps ON Properties.PNo=Maps.PNo INNER JOIN Floors ON Maps.PNo=Floors.PNo AND Maps.FNo=Floors.FNo LEFT JOIN XData ON Maps.PNo=XData.PNo AND Maps.DXFName=XData.FileName WHERE Maps.MNo=" & MapNumber & " AND (Floors.PUBable=1 OR Floors.PUBable IS NULL)  AND (Maps.D2 Is Null) AND (DATALENGTH(Map)>0) AND (Properties.PNo='" & Propertynumber & "') ORDER BY Elevation"
        Else
            TSQL = "SELECT Maps.Name, Properties.PName, Maps.MNo, Maps.DXFScale, Maps.Size1, Maps.Size2, Maps.Size3, Maps.Size4,Maps.Style4, CONVERT(nvarchar(50), Maps.SavedOn, 120) AS NOW, XData.XNo, Floors.PNo, Floors.FNo, Floors.Elevation, Maps.Type FROM Properties INNER JOIN Maps ON Properties.PNo=Maps.PNo INNER JOIN Floors ON Maps.PNo=Floors.PNo AND Maps.FNo=Floors.FNo LEFT JOIN XData ON Maps.PNo=XData.PNo AND Maps.DXFName=XData.FileName WHERE (Floors.PUBable=1 OR Floors.PUBable IS NULL) AND (Maps.Type=0) AND (Maps.D2 Is Null) AND (DATALENGTH(Map)>0) AND (Properties.PNo='" & Propertynumber & "') AND Floors.FNo='" & FNo & "' ORDER BY Elevation"
        End If
        Dim DT As DataTable = GetTable(Session("Connection"), TSQL)
        If DT.Rows.Count < 1 Then
            'only Master drawing
            TSQL = "SELECT Maps.Name, Properties.PName, Maps.MNo, Maps.DXFScale, Maps.Size1, Maps.Size2, Maps.Size3, Maps.Size4,Maps.Style4, CONVERT(nvarchar(50), Maps.SavedOn, 120) AS NOW, XData.XNo, Floors.PNo, Floors.FNo, Floors.Elevation, Maps.Type FROM Properties INNER JOIN Maps ON Properties.PNo=Maps.PNo INNER JOIN Floors ON Maps.PNo=Floors.PNo AND Maps.FNo=Floors.FNo LEFT JOIN XData ON Maps.PNo=XData.PNo AND Maps.DXFName=XData.FileName WHERE (Floors.PUBable=1 OR Floors.PUBable IS NULL) AND (Maps.Type=0) AND (Maps.D2 Is Null) AND (DATALENGTH(Map)>0) AND (Properties.PNo='" & Propertynumber & "') AND Floors.FNo='" & FNo & "' ORDER BY Elevation"
            DT = GetTable(Session("Connection"), TSQL)
        End If
        If DT.Rows.Count > 0 Then
            If Request.QueryString("customary") IsNot Nothing AndAlso Request.QueryString("customary").Trim().Length > 0 Then Customary = Request.QueryString("customary").Trim()
            '  If IsNumeric(GetValue(Session("Connection"), "SELECT Value FROM Settings WHERE name='Customary'")) Then Customary = GetValue(Session("Connection"), "SELECT Value FROM Settings WHERE name='Customary'")
            DimensionFormat = DT(0)("Style4")
            Symbol(1) = Color.Black
            Symbol(4) = Color.Black
            Symbol(9) = Color.Black
            Symbol(16) = Color.Black
            If Customary And 32 Then
                TSQL = "SELECT * FROM " & Session("LEX") & "Symbols"
                Dim ST As DataTable = GetTable(Session("Connection"), TSQL)
                For Each SR As DataRow In ST.Rows
                    Symbol(SR("Actiontype")) = Color.FromArgb(SR("Color"))
                Next
            End If

            PropertyName = DT(0)("PName")
            LoadMap(DT(0)("MNo"))
            labelsize = DT(0)("Size3") * 1.5
            Dim dx As Single = Math.Abs(fx2 - fx1)
            Dim dy As Single = Math.Abs(fy2 - fy1)
            Dim aspect As Single = CDbl(svgX) / svgY
            If dx > dy * aspect Then
                dy = dx / aspect
            Else
                dx = dy * aspect
            End If
            Dim sx1 As Single = (fx1 + fx2) / 2 - dx / 2
            Dim sx2 As Single = sx1 + dx
            Dim sy1 As Single = (fy1 + fy2) / 2 - dy / 2
            Dim sy2 As Single = sy1 + dy
            sx = svgX * 10 / (sx2 - sx1)
            sy = svgY * 10 / (sy1 - sy2)
            ox = sx1
            oy = sy2
            Dim VName As String = GetValue(Session("Connection"), "SELECT VName FROM " & Session("LEX") & "Themes WHERE VNo=" & ViewNumber)
            Response.Output.WriteLine("<?xml version='1.0' encoding='UTF-8'?>")
            Response.Output.WriteLine("<!DOCTYPE html>")
            Response.Output.WriteLine("<html>")
            Response.Output.WriteLine("<head><meta http-equiv=" & Chr(34) & "X-UA-Compatible" & Chr(34) & " content=" & Chr(34) & "IE=9" & Chr(34) & "/><link href=" & Chr(34) & "Styles/Site.css" & Chr(34) & " rel=" & Chr(34) & "stylesheet" & Chr(34) & " type=" & Chr(34) & "text/css" & Chr(34) & " /><script language=" & Chr(34) & "javascript" & Chr(34) & " type=" & Chr(34) & "text/javascript" & Chr(34) & " src='Scripts/Tools.js'></script></head>")
            Response.Output.WriteLine("<body width='960px' height='100%'>")
            Response.Output.WriteLine("<div align=" & Chr(34) & "center" & Chr(34) & " >")
            Response.Output.WriteLine("<table width='" & svgX & "px' height='100%'><tr><td width='80px' align='center' style='overflow:hidden;'>")
            Response.Output.WriteLine("<div style=' float:left; margin-left:10px; height:30px; text-align:center; width:80px;'>")
            Response.Output.WriteLine("<svg id='zoom' xmlns='http://www.w3.org/2000/svg'  >   <g id='minus' pointer-events='all' onmousedown='zoomout(evt)'>  <circle cx='10' cy='10' r='8' stroke='black' fill='yellow' />      <line stroke='black' x1='5' x2='15' y1='10' y2='10' />   </g> <g></g>    <g id='plus' pointer-events='all' onmousedown='zoomin(evt)'>      <circle cx='50' cy='10' r='8' stroke='black' fill='yellow' />      <line stroke='black' x1='45' x2='55' y1='10' y2='10' />      <line stroke='black' x1='50' x2='50' y1='5' y2='15' />   </g> </svg>")
            Response.Output.WriteLine("</div><div class='headerLegend' style='float:left;' >" & VName & "</div></td></tr>")
            Response.Output.WriteLine("<tr><td width='" & svgX & "px' align='left' style='overflow:hidden;'><svg id='root' width='" & svgX & "px' height='" & svgY & "px' viewBox='0 0 " & svgX & "0 " & svgY & "0' onmousedown='handleMouseDown(evt);' onmousemove='handleMouseMove(evt);' onmouseup='handleMouseUp(evt);'>")
            '1. Fill Spaces
            Dim stroke As String = "Black"
            BuildDrawFlex1("drawCol")
            Dim c As Integer = 0
            Dim n As Integer = 0
            Dim Buffer As String = ""
            Dim fill As String = "#FFFFFF"
            Dim AvailTypes As New Dictionary(Of String, Integer)
            Dim cviewTable As New Dictionary(Of String, Integer)
            Dim colorTop10s As New List(Of colorTop10)
            Response.Output.WriteLine("<g id='rooms' pointer-events='fill' onmouseover='showTooltip(evt)' onmouseout='hideTooltip(evt)' style='overflow:hidden;'>")
            If ViewType = "M" Then
                For g As Integer = 1 To UBound(rg)
                    Dim irec As Integer = rg(g).ARec
                    Dim RoomID As String = Trim(fg(irec).ID)
                    If Len(Trim(RoomID)) Then
                        Dim k As Integer = 255
                        For j As Integer = 0 To detailsList.Count - 1
                            If detailsList(j).id = irec Then
                                k = 255
                                If Len(Trim(detailsList(j).LNo)) > 0 Then
                                    If Len(Trim(detailsList(j).VQNo)) > 0 Then
                                        If colorTop10s.Count > 0 Then
                                            Dim colorTop As colorTop10
                                            Session("ID") = detailsList(j).VQNo
                                            colorTop = colorTop10s.Find(AddressOf findCategories)

                                            If colorTop IsNot Nothing Then
                                                If Len(Trim(Session("QNo1") & "")) > 0 Then
                                                    colorTop10s.Find(Function(t As colorTop10) t.description = detailsList(j).VQNo).RSF += CInt(detailsList(j).Q1)
                                                Else
                                                    colorTop10s.Find(Function(t As colorTop10) t.description = detailsList(j).VQNo).RSF += detailsList(j).Rentable
                                                End If
                                            Else
                                                If Len(Trim(Session("QNo1") & "")) > 0 Then
                                                    colorTop10s.Add(New colorTop10(detailsList(j).UNo, HEXColor(k), detailsList(j).VQNo, CInt(detailsList(j).Q1)))
                                                Else
                                                    colorTop10s.Add(New colorTop10(detailsList(j).UNo, HEXColor(k), detailsList(j).VQNo, detailsList(j).Rentable))
                                                End If
                                            End If
                                        Else
                                            If Len(Trim(Session("QNo1") & "")) > 0 Then
                                                colorTop10s.Add(New colorTop10(detailsList(j).UNo, HEXColor(k), detailsList(j).VQNo, CInt(detailsList(j).Q1)))
                                            Else
                                                colorTop10s.Add(New colorTop10(detailsList(j).UNo, HEXColor(k), detailsList(j).VQNo, detailsList(j).Rentable))
                                            End If
                                        End If
                                    End If
                                Else

                                End If
                            End If
                        Next
                    End If
                Next
                Dim keys As List(Of colorTop10) = colorTop10s
                keys.Sort(Function(v1, v2) v1.RSF.CompareTo(v2.RSF))
                colorTop10s = New List(Of colorTop10)
                Dim top As Integer = 0
                If keys.Count > 0 Then
                    For m = keys.Count - 1 To 0 Step -1
                        If top < 9 Then
                            colorTop10s.Add(New colorTop10("", "", "", 0))
                            colorTop10s(top).color = HEXColor(ViewColor(top + 1))
                            colorTop10s(top).description = keys(m).description
                            colorTop10s(top).RSF = keys(m).RSF
                            top += 1
                        Else
                            Exit For
                        End If
                    Next
                End If
            ElseIf ViewType = "C" Then
            End If
            For g As Integer = 1 To UBound(rg)
                Dim irec As Integer = rg(g).ARec
                If irec <= UBound(fg) Then
                    ReDim circuit(0)
                    Call sloop(0, rg(g).BRec)
                    If UBound(circuit) Then
                        Dim RoomID As String = Trim(fg(irec).ID)
                        Dim labels As String = ""
                        If Len(Trim(RoomID)) Then
                            Dim k As Integer = 255
                            For j As Integer = 0 To detailsList.Count - 1
                                If detailsList(j).id = irec Then
                                    k = 0
                                    If Len(Trim(detailsList(j).LNo)) > 0 Then
                                        labels = detailsList(j).OQN0 & " " & oSuffix(0) & "^" & detailsList(j).OQN1 & " " & oSuffix(1) & "^" & detailsList(j).OQN2 & " " & oSuffix(2) & "^" & detailsList(j).OQN3 & " " & oSuffix(3)
                                    Else
                                        labels = detailsList(j).VQN0 & " " & vSuffix(0) & "^" & detailsList(j).VQN1 & " " & vSuffix(1) & "^" & detailsList(j).VQN2 & " " & vSuffix(2) & "^" & detailsList(j).VQN3 & " " & vSuffix(3)
                                        fill = "#C0C0C0"
                                    End If
                                    'Get View colors 
                                    fill = IpadViews(detailsList, j, g, n, VSET, Propertynumber, cviewTable, colorTop10s)
                                    'svgLabel's color rooms
                                    If (Customary And 1) Or (Customary And 128) Then
                                        For cst As Integer = 0 To Custom.Count - 1
                                            If Custom(cst).RNo = rg(g).ARec And Custom(cst).Hatch > 0 Then
                                                If ((Custom(cst).Type = 0) And (Customary And 1)) Or ((Custom(cst).Type = 1) And (Customary And 128)) Then
                                                    Dim P As Integer = Custom(cst).Color
                                                    If P > 0 And P < 10 Then fill = HEXColor(ViewColor(P))
                                                    If P = Highlight Then fill = HEXColor(Highlight.ToString())
                                                End If
                                            End If
                                        Next
                                    End If
                                End If
                            Next
                            If k = 255 Then fill = "#C0C0C0"
                            Buffer = "<path id='" & svgEsc(RoomID) & "' stroke='none' fill='" & fill & "' desc='" & svgEsc(labels) & "' d='M"
                            Dim x As String = CStr(CLng((circuit(1).X - ox) * sx))
                            Dim y As String = CStr(CLng((circuit(1).Y - oy) * sy))
                            Buffer = Buffer & " " & x & "," & y & " "
                            For L As Integer = 2 To UBound(circuit) - 1
                                x = CStr(CLng((circuit(L).X - ox) * sx))
                                y = CStr(CLng((circuit(L).Y - oy) * sy))
                                Buffer = Buffer & " " & x & "," & y
                                Buffer = Buffer & " " & x & "," & y & " "
                            Next
                            Response.Output.WriteLine(Buffer & " Z'/>")
                        Else
                            fill = "#FFFFFF"
                            Buffer = "<path stroke='none' fill='" & fill & "' desc='" & svgEsc(labels) & "' d='M"
                            Dim x As String = CStr(CLng((circuit(1).X - ox) * sx))
                            Dim y As String = CStr(CLng((circuit(1).Y - oy) * sy))
                            Buffer = Buffer & " " & x & "," & y & " "
                            For L As Integer = 2 To UBound(circuit) - 1
                                x = CStr(CLng((circuit(L).X - ox) * sx))
                                y = CStr(CLng((circuit(L).Y - oy) * sy))
                                Buffer = Buffer & " " & x & "," & y
                            Next
                            Response.Output.WriteLine(Buffer & " Z'/> ")
                        End If
                    End If
                End If
            Next
            Response.Output.WriteLine("</g")

            '2. Draw Boundaries
            If Customary And 8 Then
                Dim wall As Single = 6 * sx / DXFScale
                If Customary And 32 Then stroke = HEXColor(Symbol(1).ToArgb)
                For g As Integer = 1 To UBound(rg)
                    Dim irec As Integer = rg(g).ARec
                    If irec <= UBound(fg) Then
                        ReDim circuit(0)
                        Call sloop(0, rg(g).BRec)
                        If UBound(circuit) Then
                            Buffer = "<path fill='none' stroke='" & stroke & "' stroke-width='" & wall & "' d='M"
                            Dim x As String = CStr(CLng((circuit(1).X - ox) * sx))
                            Dim y As String = CStr(CLng((circuit(1).Y - oy) * sy))
                            Buffer = Buffer & " " & x & "," & y & " L"
                            For L As Integer = 2 To UBound(circuit) - 1
                                x = CStr(CLng((circuit(L).X - ox) * sx))
                                y = CStr(CLng((circuit(L).Y - oy) * sy))
                                Buffer = Buffer & " " & x & "," & y
                            Next
                            Response.Output.WriteLine(Buffer & " Z' />")
                        End If
                    End If
                Next
            End If

            '3. Draw Background
            SVGBackground()

            '4. draw dimensions and markers

            If Customary And 16 Then SVGDimensions()
            '5. draw tenant logos

            If Customary And 128 Then SVGTenantLogo()
            '6. draw room labels and custom labels

            SVGLabelRooms()

            'Custom text
            If Customary And 2 Then SVGTextBox()
            'draw label box
            For i As Integer = 1 To UBound(fg)
                If (fg(i).C1 And 15) = 14 Then
                    SVGPicTable(i)
                    Exit For
                End If
            Next
            Response.Output.WriteLine("<g id='tooltip' opacity='0.8' visibility='hidden' pointer-events='none'> style='overflow:hidden;float:center;'")
            Response.Output.WriteLine("  <rect id='toolrect' x='0' y='0' width='0' height='0' rx='2' ry='2' fill='lightyellow' stroke='black' />")
            Response.Output.WriteLine("  <text id='tooltext' x='0' y='0' font-family='arial' font-size='11' >")
            Response.Output.WriteLine("     <tspan id='txt1' dx='5' dy='1.2em' font-weight='bold'>1</tspan>")
            Response.Output.WriteLine("     <tspan id='txt2' dx='5' dy='2.4em' font-weight='bold'>2</tspan>")
            Response.Output.WriteLine("     <tspan id='txt3' dx='5' dy='3.6em' font-weight='bold'>3</tspan>")
            Response.Output.WriteLine("     <tspan id='txt4' dx='5' dy='4.8em' font-weight='bold'>4</tspan>")
            Response.Output.WriteLine("  </text>")
            Response.Output.WriteLine("</g>")
            Response.Output.WriteLine("</svg></td></tr>")
            Response.Output.WriteLine("<tr style=' height:60px;width:" & svgX & "px;' align='center'>")
            Response.Output.WriteLine("<td align='center'>")
            Session("legendLists") = ""
            Session("tablewidth") = 0
            SelectedThemes(AvailTypes, VSET, Propertynumber, ViewNumber, colorTop10s)
            Dim LegendLists As String = Session("legendLists")
            If Len(Trim(Session("legendLists").ToString())) > 0 Then
                Response.Output.WriteLine("<table border='1' width='" & Session("tablewidth") & "px'><tr align='center'>")
                Response.Output.WriteLine(LegendLists)
                Response.Output.WriteLine("</tr></table>")
            End If
            Response.Output.WriteLine("</td></tr></table>")
            Response.Output.WriteLine("</div>")
            Response.Output.WriteLine("</body>")
            Response.Output.WriteLine("</html>")
        End If


    End Sub

    Private Sub SVGPicTable(ByVal irec As Integer)
        Try
            Dim z As Integer = (fg(irec).C2 / DXFScale) * sx
            Dim v As Integer = z * 2 / 3
            Dim x1 As Integer = (fg(irec).X1 - ox) * sx
            Dim y1 As Integer = (fg(irec).Y1 - oy) * sy
            Dim x2 As Integer = (fg(irec).X2 - ox) * sx
            Dim y2 As Integer = (fg(irec).Y2 - oy) * sy

            Dim wh As Single = z
            Dim wc(3) As Integer
            Dim wx(3) As Integer
            Dim ct(3) As Integer

            Dim OO As Integer = 0

            Dim N As Integer = 0
            Dim COR(detailsList.Count) As Integer
            For R As Integer = 0 To detailsList.Count - 1
                Dim s As Integer = detailsList(R).id
                If fg(s).P1 And 1024 Then
                    N = N + 1
                    COR(N) = R
                End If
            Next
            If N Then 'sort
                ReDim Preserve COR(N)
                Dim k As Integer
                For i As Integer = 1 To N - 1
                    For j As Integer = i + 1 To N
                        If Val(detailsList(COR(j)).VQN0) < Val(detailsList(COR(i)).VQN0) Then
                            k = COR(i)
                            COR(i) = COR(j)
                            COR(j) = k
                        End If
                    Next
                Next
            End If
            For N = 0 To 3
                wx(N) = 0
                wc(N) = 0
                ct(N) = 0
            Next
            Dim Buffer As String
            Dim row
            If VQN(0) Then
                row = SysData.Select("QNo=" & VQN(0))
                Buffer = Trim(row(0)(1)) ' Trim(PubFlex1.GetDataDisplay(0, SC(VQN(0))))
                wc(0) = Len(Buffer) * v
                '   If PubFlex1.Cols(SC(VQN(0))).TextAlign = C1.Win.C1FlexGrid.TextAlignEnum.RightCenter Then 
                If Trim(row(0)("colAlign")) = "R" Then ct(0) = 1
                For R As Integer = 0 To detailsList.Count - 1
                    Buffer = Trim(detailsList(R).VQN0 & "")
                    Dim w As Single = Len(Buffer) * v
                    If w > wc(0) Then wc(0) = w + 3
                Next
            End If
            wx(1) = wx(0) + wc(0) + wh / 3
            wc(1) = 0
            ct(1) = 0
            If OQN(0) = VQN(0) Then OO = 1
            If 1 - OO Then
                row = SysData.Select("QNo=" & OQN(0))
                Buffer = Trim(row(0)(1))
                wc(1) = Len(Buffer) * v
                ' If PubFlex1.Cols(SC(OQN(1 - OO))).TextAlign = C1.Win.C1FlexGrid.TextAlignEnum.RightCenter Then
                If Trim(row(0)("colAlign")) = "R" Then ct(1) = 1
                For R As Integer = 0 To detailsList.Count - 1
                    Buffer = Trim(detailsList(R).OQN0 & "")
                    Dim w As Single = Len(Buffer) * v
                    If w > wc(1) Then wc(1) = w
                Next
            End If
            wx(2) = wx(1) + wc(1) + wh / 3
            wc(2) = 0
            ct(2) = 0
            If (2 - OO) Then
                row = SysData.Select("QNo=" & OQN(1))
                Buffer = Trim(row(0)(1))
                wc(2) = Len(Buffer) * v
                '  If PubFlex1.Cols(SC(OQN(2 - OO))).TextAlign = C1.Win.C1FlexGrid.TextAlignEnum.RightCenter Then 
                If Trim(row(0)("colAlign")) = "R" Then ct(2) = 1
                For R As Integer = 0 To detailsList.Count - 1
                    Buffer = Trim(Trim(detailsList(R).OQN1) & "")
                    Dim w As Single = Len(Buffer) * v
                    If w > wc(2) Then wc(2) = w
                Next
            End If
            wx(3) = wx(2) + wc(2) + wh / 3
            wc(3) = 0
            ct(3) = 0
            If (3 - OO) Then
                row = SysData.Select("QNo=" & OQN(2))
                Buffer = Trim(row(0)(1))
                wc(3) = Len(Buffer) * v
                ' If PubFlex1.Cols(SC(OQN(3 - OO))).TextAlign = C1.Win.C1FlexGrid.TextAlignEnum.RightCenter Then 
                If Trim(row(0)("colAlign")) = "R" Then ct(3) = 1
                For R As Integer = 0 To detailsList.Count - 1
                    Buffer = Trim(Trim(detailsList(R).OQN2) & "")
                    Dim w As Single = Len(Buffer) * v
                    If w > wc(3) Then wc(3) = w
                Next
            End If

            x2 = x1 + wx(3) + wc(3) + wh

            Response.Output.WriteLine("<rect x='" & x1 & "' y='" & y2 & "' width='" & x2 - x1 & "' height='" & y1 - y2 & "' fill='white' stroke='black' />")

            Dim TX As Single = x1 + wh / 2
            Dim TY As Single = y2 + wh * 3 / 2

            Dim TT As String = ""
            If wc(0) Then
                row = SysData.Select("QNo=" & VQN(0))
                TT = Trim(row(0)(1))
                If ct(0) Then
                    Response.Output.WriteLine("<text x='" & TX + wc(0) & "' y='" & TY & "' " & " fill='black' font-size='" & z & "' text-anchor='end' pointer-events='none'>" & svgEsc(TT) & "</text>")
                Else
                    Response.Output.WriteLine("<text x='" & TX & "' y='" & TY & "' " & " fill='black' font-size='" & z & "' text-anchor='start' pointer-events='none'>" & svgEsc(TT) & "</text>")
                End If
            End If
            If wc(1) Then
                row = SysData.Select("QNo=" & OQN(0))
                TT = Trim(row(0)(1))
                If ct(1) Then
                    Response.Output.WriteLine("<text x='" & TX + wx(1) + wc(1) & "' y='" & TY & "' " & " fill='black' font-size='" & z & "' text-anchor='end' pointer-events='none'>" & svgEsc(TT) & "</text>")
                Else
                    Response.Output.WriteLine("<text x='" & TX + wx(1) & "' y='" & TY & "' " & " fill='black' font-size='" & z & "' text-anchor='start' pointer-events='none'>" & svgEsc(TT) & "</text>")
                End If
            End If
            If wc(2) Then
                row = SysData.Select("QNo=" & OQN(1))
                TT = Trim(row(0)(1))
                If ct(2) Then
                    Response.Output.WriteLine("<text x='" & TX + wx(2) + wc(2) & "' y='" & TY & "' " & " fill='black' font-size='" & z & "' text-anchor='end' pointer-events='none'>" & svgEsc(TT) & "</text>")
                Else
                    Response.Output.WriteLine("<text x='" & TX + wx(2) & "' y='" & TY & "' " & " fill='black' font-size='" & z & "' text-anchor='start' pointer-events='none'>" & svgEsc(TT) & "</text>")
                End If
            End If
            If wc(3) Then
                row = SysData.Select("QNo=" & OQN(2))
                TT = Trim(row(0)(1))
                If ct(3) Then
                    Response.Output.WriteLine("<text x='" & TX + wx(3) + wc(3) & "' y='" & TY & "' " & " fill='black' font-size='" & z & "' text-anchor='end' pointer-events='none'>" & svgEsc(TT) & "</text>")
                Else
                    Response.Output.WriteLine("<text x='" & TX + wx(3) & "' y='" & TY & "' " & " fill='black' font-size='" & z & "' text-anchor='start' pointer-events='none'>" & svgEsc(TT) & "</text>")
                End If
            End If
            TY = TY + wh * 1.5
            ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
            For N = 1 To UBound(COR)
                Dim R As Integer = COR(N)
                Dim S As Integer = detailsList(R).id
                If fg(S).P1 And 1024 Then
                    TT = Trim(detailsList(R).VQN0)
                    If ct(0) Then
                        Response.Output.WriteLine("<text x='" & TX + wc(0) & "' y='" & TY & "' " & " fill='black' font-size='" & z & "' text-anchor='end' pointer-events='none'>" & svgEsc(TT) & "</text>")
                    Else
                        Response.Output.WriteLine("<text x='" & TX & "' y='" & TY & "' " & " fill='black' font-size='" & z & "' text-anchor='start' pointer-events='none'>" & svgEsc(TT) & "</text>")
                    End If
                    If Len(Trim(detailsList(R).LNo)) Then
                        If OQN(0) = VQN(0) Then OO = 1
                        If 1 - OO Then
                            TT = Trim(detailsList(R).OQN0)
                            If ct(1) Then
                                Response.Output.WriteLine("<text x='" & TX + wx(1) + wc(1) & "' y='" & TY & "' " & " fill='black' font-size='" & z & "' text-anchor='end' pointer-events='none'>" & svgEsc(TT) & "</text>")
                            Else
                                Response.Output.WriteLine("<text x='" & TX + wx(1) & "' y='" & TY & "' " & " fill='black' font-size='" & z & "' text-anchor='start' pointer-events='none'>" & svgEsc(TT) & "</text>")
                            End If
                        End If
                        If (2 - OO) Then
                            TT = Trim(detailsList(R).OQN1)
                            If ct(2) Then
                                Response.Output.WriteLine("<text x='" & TX + wx(2) + wc(2) & "' y='" & TY & "' " & " fill='black' font-size='" & z & "' text-anchor='end' pointer-events='none'>" & svgEsc(TT) & "</text>")
                            Else
                                Response.Output.WriteLine("<text x='" & TX + wx(2) & "' y='" & TY & "' " & " fill='black' font-size='" & z & "' text-anchor='start' pointer-events='none'>" & svgEsc(TT) & "</text>")
                            End If
                        End If
                        If (3 - OO) Then
                            TT = Trim(detailsList(R).OQN2)
                            If ct(3) Then
                                Response.Output.WriteLine("<text x='" & TX + wx(3) + wc(3) & "' y='" & TY & "' " & " fill='black' font-size='" & z & "' text-anchor='end' pointer-events='none'>" & svgEsc(TT) & "</text>")
                            Else
                                Response.Output.WriteLine("<text x='" & TX + wx(3) & "' y='" & TY & "' " & " fill='black' font-size='" & z & "' text-anchor='start' pointer-events='none'>" & svgEsc(TT) & "</text>")
                            End If
                        End If
                    Else
                        If VQN(1) Then
                            TT = Trim(detailsList(R).VQN1)
                            row = SysData.Select("QNo=" & VQN(1))
                            If Trim(row(0)("colAlign")) = "R" Then ct(1) = 1
                            If ct(1) Then
                                Response.Output.WriteLine("<text x='" & TX + wx(1) + wc(1) & "' y='" & TY & "' " & " fill='black' font-size='" & z & "' text-anchor='end' pointer-events='none'>" & svgEsc(TT) & "</text>")
                            Else
                                Response.Output.WriteLine("<text x='" & TX + wx(1) & "' y='" & TY & "' " & " fill='black' font-size='" & z & "' text-anchor='start' pointer-events='none'>" & svgEsc(TT) & "</text>")
                            End If
                        End If
                        If VQN(2) Then
                            TT = Trim(detailsList(R).VQN2)
                            row = SysData.Select("QNo=" & VQN(2))
                            If Trim(row(0)("colAlign")) = "R" Then ct(2) = 1
                            If ct(2) Then
                                Response.Output.WriteLine("<text x='" & TX + wx(2) + wc(2) & "' y='" & TY & "' " & " fill='black' font-size='" & z & "' text-anchor='end' pointer-events='none'>" & svgEsc(TT) & "</text>")
                            Else
                                Response.Output.WriteLine("<text x='" & TX + wx(2) & "' y='" & TY & "' " & " fill='black' font-size='" & z & "' text-anchor='start' pointer-events='none'>" & svgEsc(TT) & "</text>")
                            End If
                        End If
                        If VQN(3) Then
                            TT = Trim(detailsList(R).VQN3)
                            row = SysData.Select("QNo=" & VQN(3))
                            If Trim(row(0)("colAlign")) = "R" Then ct(3) = 1
                            If ct(3) Then
                                Response.Output.WriteLine("<text x='" & TX + wx(3) + wc(3) & "' y='" & TY & "' " & " fill='black' font-size='" & z & "' text-anchor='end' pointer-events='none'>" & svgEsc(TT) & "</text>")
                            Else
                                Response.Output.WriteLine("<text x='" & TX + wx(3) & "' y='" & TY & "' " & " fill='black' font-size='" & z & "' text-anchor='start' pointer-events='none'>" & svgEsc(TT) & "</text>")
                            End If
                        End If
                    End If
                    TY = TY + wh * 1.75
                End If
            Next
        Catch ex As Exception
            MsgBox(ex.Message)
        End Try
    End Sub

    Public Sub SVGTextBox()

        Dim x1, y1, x2, y2 As Integer
        Dim c As Single ' character width
        Dim w As Single ' box width
        Dim z As Single ' text size
        For i As Integer = 1 To UBound(fg)
            If (fg(i).C1 And 15) = 12 Then
                For n As Integer = 1 To UBound(Custom)
                    If Custom(n).RNo = i Then
                        Dim textstring As String = Custom(n).Label
                        x1 = (fg(i).X1 - ox) * sx
                        y1 = (fg(i).Y1 - oy) * sy
                        x2 = (fg(i).X2 - ox) * sx
                        y2 = (fg(i).Y2 - oy) * sy
                        If fg(i).P1 >= 30 Then
                            z = fg(i).P1 / DXFScale
                        Else
                            z = (fg(i).P1 + 1) * 30 / DXFScale
                        End If
                        z = z * sx * 1.1 'good
                        c = z * 0.45
                        Dim temp As String = ""
                        Dim word As String = ""
                        Dim plus As String = ""
                        If fg(i).P2 = 0 Then
                            w = x2 - x1
                            x1 = x1 + c / 2
                            y2 = y2 + z
                            For o As Integer = 1 To Len(textstring)
                                Dim t As String = Mid(textstring, o, 1)
                                Select Case t
                                    Case Chr(10)
                                    Case Chr(13)
                                        If Len(temp) Then Response.Output.WriteLine("<text fill='black' font-family='Arial' font-size='" & z & "' x='" & x1 & "' y='" & y2 & "' pointer-events='none'>" & svgEsc(Trim(temp)) & "</text>")
                                        word = ""
                                        temp = ""
                                        plus = ""
                                        y2 = y2 + z
                                    Case Chr(32)
                                        word = temp
                                        temp &= t
                                        plus = ""
                                    Case Else
                                        temp &= t
                                        plus &= t
                                End Select
                                If Len(temp) * c > w Then
                                    If Len(word) Then
                                        Response.Output.WriteLine("<text fill='black' font-family='Arial' font-size='" & z & "' x='" & x1 & "' y='" & y2 & "' pointer-events='none'>" & svgEsc(Trim(word)) & "</text>")
                                        word = ""
                                        temp = plus
                                        plus = ""
                                        y2 = y2 + z
                                    End If
                                End If
                            Next
                            Response.Output.WriteLine("<text fill='black' font-family='Arial' font-size='" & z & "' x='" & x1 & "' y='" & y2 & "' pointer-events='none'>" & svgEsc(Trim(temp)) & "</text>")
                        ElseIf fg(i).P2 = 1 Then 'upwards
                            w = y1 - y2
                            x1 = x1 + z
                            y1 = y1 - c / 2
                            For o As Integer = 1 To Len(textstring)
                                Dim t As String = Mid(textstring, o, 1)
                                Select Case t
                                    Case Chr(10)
                                    Case Chr(13)
                                        temp = Trim(temp)
                                        If Len(temp) Then Response.Output.WriteLine("<text fill='black' font-family='Arial' font-size='" & z & "' x='" & x1 & "' y='" & y1 & "' transform='translate(" & x1 & "," & y1 & ") rotate(-90) translate(-" & x1 & ",-" & y1 & ")' pointer-events='none'>" & svgEsc(Trim(temp)) & "</text>")
                                        word = ""
                                        temp = ""
                                        plus = ""
                                        x1 = x1 + z
                                    Case Chr(32)
                                        word = temp
                                        temp &= t
                                        plus = ""
                                    Case Else
                                        temp &= t
                                        plus &= t
                                End Select
                                If Len(temp) * c > w Then
                                    If Len(word) Then
                                        Response.Output.WriteLine("<text fill='black' font-family='Arial' font-size='" & z & "' x='" & x1 & "' y='" & y1 & "' transform='translate(" & x1 & "," & y1 & ") rotate(-90) translate(-" & x1 & ",-" & y1 & ")' pointer-events='none'>" & svgEsc(Trim(word)) & "</text>")
                                        word = ""
                                        temp = plus
                                        plus = ""
                                        x1 = x1 + z
                                    End If
                                End If
                            Next
                            Response.Output.WriteLine("<text fill='black' font-family='Arial' font-size='" & z & "' x='" & x1 & "' y='" & y1 & "' transform='translate(" & x1 & "," & y1 & ") rotate(-90) translate(-" & x1 & ",-" & y1 & ")' pointer-events='none'>" & svgEsc(Trim(temp)) & "</text>")

                        ElseIf fg(i).P2 = 2 Then 'downwards
                            w = y1 - y2
                            x2 = x2 - z
                            y2 = y2 + c / 2
                            For o As Integer = 1 To Len(textstring)
                                Dim t As String = Mid(textstring, o, 1)
                                Select Case t
                                    Case Chr(10)
                                    Case Chr(13)
                                        If Len(temp) Then Response.Output.WriteLine("<text fill='black' font-family='Arial' font-size='" & z & "' x='" & x2 & "' y='" & y2 & "' transform='translate(" & x2 & "," & y2 & ") rotate(90) translate(-" & x2 & ",-" & y2 & ")' pointer-events='none'>" & svgEsc(Trim(temp)) & "</text>")
                                        word = ""
                                        temp = ""
                                        plus = ""
                                        x2 = x2 - z
                                    Case Chr(32)
                                        word = temp
                                        temp &= t
                                        plus = ""
                                    Case Else
                                        temp &= t
                                        plus &= t
                                End Select
                                If Len(temp) * c > w Then
                                    If Len(word) Then
                                        Response.Output.WriteLine("<text fill='black' font-family='Arial' font-size='" & z & "' x='" & x2 & "' y='" & y2 & "' transform='translate(" & x2 & "," & y2 & ") rotate(90) translate(-" & x2 & ",-" & y2 & ")' pointer-events='none'>" & svgEsc(Trim(word)) & "</text>")
                                        word = ""
                                        temp = plus
                                        plus = ""
                                        x2 = x2 - z
                                    End If
                                End If
                            Next
                            Response.Output.WriteLine("<text fill='black' font-family='Arial' font-size='" & z & "' x='" & x2 & "' y='" & y2 & "' transform='translate(" & x2 & "," & y2 & ") rotate(90) translate(-" & x2 & ",-" & y2 & ")' pointer-events='none'>" & svgEsc(Trim(temp)) & "</text>")

                        Else
                            Dim d As Integer = fg(i).P2
                            Dim co As Single = Math.Cos(d * piX2 / 360)
                            Dim si As Single = Math.Sin(d * piX2 / 360)
                            Dim x As Single = x1 + z * si
                            Dim y As Single = y1 + z * co
                            Response.Output.WriteLine("<text fill='black' font-family='Arial' font-size='" & z & "' x='" & x & "' y='" & y & "' transform='translate(" & x & "," & y & ") rotate(-" & d & ") translate(-" & x & ",-" & y & ")' pointer-events='none'>" & svgEsc(Trim(textstring)) & "</text>")
                        End If
                        Exit For
                    End If
                Next
            End If
        Next
    End Sub


    Public Shared Function Express(ByVal distance As Single) As String
        If UofM = "M" Then
            Express = Format(distance / 39.37, "#,###.000 m")
            Exit Function
        End If
        Dim feet As Long, inch As Integer
        Dim frac As Single
        Dim quar As String
        feet = Int(distance / 12)
        inch = Int(distance - (feet * 12))
        frac = distance - Int(distance)
        If frac > 0.875 Then
            quar = Chr(34)
            inch = inch + 1
            If inch > 11 Then
                inch = 0
                feet = feet + 1
            End If
        ElseIf frac > 0.625 Then
            quar = Chr(190) & Chr(34)
        ElseIf frac > 0.375 Then
            quar = Chr(189) & Chr(34)
        ElseIf frac > 0.125 Then
            quar = Chr(188) & Chr(34)
        Else
            quar = Chr(34)
        End If
        Express = CStr(feet) & Chr(39) & Chr(45) & CStr(inch) & quar
    End Function

    Public Sub SVGDimensions()
        Dim fill As String = "black"
        Dim limn As String = "black"
        If Customary And 32 Then
            If Symbol(4).ToArgb Then
                fill = HEXColor(Symbol(4).ToArgb)
                limn = HEXColor(Symbol(4).ToArgb)
            End If
        End If
        Dim S As Integer = 1.3 * FontSize4 / DXFScale
        For i As Integer = 1 To UBound(fg)
            If (fg(i).C1 And 15) = 4 Then
                Dim textstring As String = ""
                Dim x1 As Single = fg(i).X1
                Dim y1 As Single = fg(i).Y1
                Dim x2 As Single = fg(i).X2
                Dim y2 As Single = fg(i).Y2
                Dim dx As Single = x2 - x1
                Dim dy As Single = y2 - y1
                Dim dz As Single = Math.Sqrt(dx * dx + dy * dy)
                Select Case DimensionFormat
                    Case 0
                        textstring = Format(CLng(dz * DXFScale / 12), "#,##0") & "'"
                    Case 1
                        textstring = Format(dz * DXFScale / 12, "#,##0.0") & "'"
                    Case 2
                        textstring = Express(CLng(dz * DXFScale))
                    Case 3
                        textstring = Express(dz * DXFScale)
                    Case 4
                        textstring = Format(dz * DXFScale / 39.37, "#,##0.000") & "m"
                End Select
                Dim d As Single = angle(x1, y1, x2, y2) / 60
                If dx = 0 Then d = 90
                If dx < 0 Then d = d + 180
                If d > 360 Then d = d - 360

                dx = (x1 + x2) / 2
                dy = (y1 + y2) / 2
                If fg(i).C2 And 1 Then 'above the line
                    dx = dx - Math.Sin(piX2 * d / 360) * S / 5
                    dy = dy + Math.Cos(piX2 * d / 360) * S / 5
                Else
                    dx = dx + Math.Sin(piX2 * d / 360) * S
                    dy = dy - Math.Cos(piX2 * d / 360) * S
                End If
                Dim ix1 As Integer = (dx - ox) * sx
                Dim iy1 As Integer = (dy - oy) * sy
                Dim ix2 As Integer
                Dim iy2 As Integer
                Dim matrix As String
                If d = 0 Then
                    matrix = ""
                Else
                    matrix = "transform='translate(" & CStr(ix1) & "," & CStr(iy1) & ") rotate(" & CStr(360 - d) & ") translate(-" & CStr(ix1) & ",-" & CStr(iy1) & ")'"
                End If
                Response.Output.WriteLine("<text fill='" & fill & "' font-familly='Arial' font-size='" & S * sx & "' x='" & ix1 & "' y='" & iy1 & "' " & matrix & " text-anchor='middle' pointer-events='none'>" & svgEsc(Trim(textstring)) & "</text>")
                If fg(i).C2 And 2 Then
                    ix1 = (x1 - ox) * sx
                    iy1 = (y1 - oy) * sy
                    ix2 = (x2 - ox) * sx
                    iy2 = (y2 - oy) * sy
                    Response.Output.WriteLine("<line x1='" & ix1 & "' y1='" & iy1 & "' x2='" & ix2 & "' y2='" & iy2 & "' stroke='" & limn & "'/>")
                    dx = Math.Cos(piX2 * (d + 45) / 360) * S * sx / 2
                    dy = Math.Sin(piX2 * (d + 45) / 360) * S * sx / 2
                    Response.Output.WriteLine("<line x1='" & ix1 - dx & "' y1='" & iy1 + dy & "' x2='" & ix1 + dx & "' y2='" & iy1 - dy & "' stroke='" & limn & "'/>")
                    Response.Output.WriteLine("<line x1='" & ix2 - dx & "' y1='" & iy2 + dy & "' x2='" & ix2 + dx & "' y2='" & iy2 - dy & "' stroke='" & limn & "'/>")
                End If
            End If
        Next
    End Sub

    Private Sub SVGLabelRooms()
        Dim fill As String = "black"
        If Customary And 32 Then
            If Symbol(9).ToArgb Then
                fill = HEXColor(Symbol(9).ToArgb)
            End If
        End If
        Dim piX2 As Single = Math.Atan(1) * 8
        Dim Lex As String = Session("LEX")
        Dim Connection As String = Session("Connection")
        TSQL = "SELECT a.UNo,a.FNo, b.LabelType FROM Units a INNER JOIN " & Lex & "UnitTypes b ON a.UType=b.UType WHERE a.PNo='" & Propertynumber & "'"
        Dim UT As DataTable = GetTable(Connection, TSQL)
        Dim x1, y1, x2, y2 As Single
        Dim a, z As Single
        Dim N As Integer
        Dim label(4) As String
        Dim x, y As Single

        For r As Integer = 0 To detailsList.Count - 1
            Dim s As Integer = detailsList(r).id
            If (s > 0) And (s <= UBound(fg)) Then
                Dim RoomID As String = Trim(fg(s).ID)
                Dim UR() As DataRow = UT.Select("UNo='" & RoomID & "' AND FNO='" & FNo & "'")
                ReDim label(4)
                If UR.Length = 0 Then
                    z = FontSize3 / DXFScale
                Else
                    Select Case UR(0)("LabelType")
                        Case 1
                            z = FontSize1 / DXFScale
                        Case 2
                            z = FontSize2 / DXFScale
                        Case Else
                            z = FontSize3 / DXFScale
                    End Select
                End If
                Dim Regular As Boolean = True
                For N = 1 To UBound(Custom)
                    If Custom(N).RNo = s Then
                        If Custom(N).Type = 0 And (Customary And 1) Then Regular = False
                        If Custom(N).Type = 1 And (Customary And 128) Then Regular = False
                        Exit For
                    End If
                Next
                N = 0

                If Regular Then                     'Not Custom - go ahead and label it
                    If fg(s).P1 And 1024 Then       'Called Out
                        label(N) = detailsList(r).UNo 'SC(VQN(0)))
                        N += 1
                    Else
                        If Len(detailsList(r).LNo) Then 'Occupied
                            If Len(Trim(detailsList(r).OQN0 & " " & oSuffix(0))) Then
                                label(N) = Trim(detailsList(r).OQN0)
                                If Customary And 4 Then label(N) &= " " & oSuffix(0)
                                N += 1
                            End If
                            If Len(Trim(detailsList(r).OQN1 & " " & oSuffix(1))) Then
                                label(N) = Trim(detailsList(r).OQN1)
                                If Customary And 4 Then label(N) &= " " & oSuffix(1)
                                N += 1
                            End If
                            If Len(Trim(detailsList(r).OQN2 & " " & oSuffix(2))) Then
                                label(N) = Trim(detailsList(r).OQN2)
                                If Customary And 4 Then label(N) &= " " & oSuffix(2)
                                N += 1
                            End If
                            If Len(Trim(detailsList(r).OQN3 & " " & oSuffix(3))) Then
                                label(N) = Trim(detailsList(r).OQN3)
                                If Customary And 4 Then label(N) &= " " & oSuffix(3)
                                N += 1
                            End If
                        Else                         'Vacant
                            If Len(Trim(detailsList(r).VQN0 & " " & vSuffix(0))) Then
                                label(N) = Trim(detailsList(r).VQN0)
                                If Customary And 4 Then label(N) &= " " & vSuffix(0)
                                N += 1
                            End If
                            If Len(Trim(detailsList(r).VQN1 & " " & vSuffix(1))) Then
                                label(N) = Trim(detailsList(r).VQN1)
                                If Customary And 4 Then label(N) &= " " & vSuffix(1)
                                N += 1
                            End If
                            If Len(Trim(detailsList(r).VQN2 & " " & vSuffix(2))) Then
                                label(N) = Trim(detailsList(r).VQN2)
                                If Customary And 4 Then label(N) &= " " & vSuffix(2)
                                N += 1
                            End If
                            If Len(Trim(detailsList(r).VQN3 & " " & vSuffix(3))) Then
                                label(N) = Trim(detailsList(r).VQN3)
                                If Customary And 4 Then label(N) &= " " & vSuffix(3)
                                N += 1
                            End If
                        End If
                    End If
                    a = fg(s).C2 / 10 + ((fg(s).P1 And 768) \ 256) * 90
                    If a > 360 Then a = a - 360
                    If N Then
                        If z > (1 / sx) Then

                            x1 = CInt((fg(s).X1 - ox) * sx)
                            y1 = CInt((fg(s).Y1 - oy) * sy)
                            x2 = CInt((fg(s).X2 - ox) * sx)
                            y2 = CInt((fg(s).Y2 - oy) * sy)

                            Dim co As Single = Math.Cos(a * piX2 / 360)
                            Dim si As Single = Math.Sin(a * piX2 / 360)

                            Dim dx As Single = fg(s).X2 - fg(s).X1
                            Dim dy As Single = fg(s).Y2 - fg(s).Y1
                            Dim dz As Integer = (6 / DXFScale) * sx
                            If dx Or dy Then
                                Response.Output.WriteLine("<line x1='" & x1 & "' y1='" & y1 & "' x2='" & x2 & "' y2='" & y2 & "' stroke='black' />")
                                Response.Output.WriteLine("<circle cx='" & x1 & "' cy='" & y1 & "' r='" & dz & "' stroke='black' fill='none' />")
                            End If

                            x = fg(s).X2 - z * si * (N - 1.5) / 2 '- z / 10
                            y = fg(s).Y2 + z * co * (N - 1.5) / 2 '+ z / 10

                            For C As Integer = 0 To N - 1
                                If Len(Trim(label(C))) > 0 Then
                                    Dim ta As String = "middle"
                                    x1 = CInt((x - ox) * sx)
                                    y1 = CInt((y - oy) * sy)
                                    If dx Or dy Then
                                        If dx < 0 Then
                                            ta = "end"
                                        Else
                                            ta = "start"
                                        End If
                                    End If
                                    Dim zz As String = ""
                                    If a Then zz = "transform='translate(" & x1 & "," & y1 & ") rotate(" & CStr(Int(360 - a)) & ") translate(-" & x1 & ",-" & y1 & ")'"
                                    Response.Output.WriteLine("<text x='" & x1 & "' y='" & y1 & "' " & zz & " fill='" & fill & "' font-family='Arial' font-size='" & CInt(z * sx) & "' text-anchor='" & ta & "' pointer-events='none'>" & svgEsc(label(C)) & "</text>")
                                    x = x + z * si
                                    y = y - z * co
                                End If
                            Next
                        End If
                    End If
                End If
            End If
        Next

        'Custom Labels

        For i As Integer = 1 To UBound(Custom)
            Dim s As Integer = Custom(i).RNo
            Dim regular As Boolean = True
            If Custom(i).Type = 0 Then
                If Customary And 1 Then regular = False
            Else
                If Customary And 128 Then regular = False
            End If
            If Not regular Then
                If s Then
                    Dim clabels() As String = Custom(i).Label.Split("|")
                    z = Custom(i).Size / DXFScale
                    a = Custom(i).Angle
                    N = 0
                    ReDim label(4)
                    For Each clabel As String In clabels
                        If Len(Trim(clabel)) Then
                            label(N) = Trim(clabel)
                            N = N + 1
                        End If
                    Next
                    If N Then
                        If z > (1 / sx) Then

                            x1 = CInt((fg(s).X1 - ox) * sx)
                            y1 = CInt((fg(s).Y1 - oy) * sy)
                            x2 = CInt((fg(s).X2 - ox) * sx)
                            y2 = CInt((fg(s).Y2 - oy) * sy)

                            Dim co As Single = Math.Cos(a * piX2 / 360)
                            Dim si As Single = Math.Sin(a * piX2 / 360)
                            x = fg(s).X2 - z * si * (N - 1) / 2 - z / 10
                            y = fg(s).Y2 + z * co * (N - 1) / 2 + z / 10
                            Dim dx As Single = fg(s).X2 - fg(s).X1
                            Dim dy As Single = fg(s).Y2 - fg(s).Y1
                            Dim dz As Integer = (6 / DXFScale) * sx
                            If dx Or dy Then
                                Response.Output.WriteLine("<line x1='" & x1 & "' y1='" & y1 & "' x2='" & x2 & "' y2='" & y2 & "' stroke='black' />")
                                Response.Output.WriteLine("<circle cx='" & x1 & "' cy='" & y1 & "' r='" & dz & "' stroke='black' fill='none' />")
                            End If
                            x = fg(s).X2 - z * si * (N - 1.5) / 2 '- z / 10
                            y = fg(s).Y2 + z * co * (N - 1.5) / 2 '+ z / 10
                            For C As Integer = 0 To N - 1
                                Dim ta As String = "middle"
                                If dx Or dy Then
                                    If dx < 0 Then
                                        ta = "end"
                                    Else
                                        ta = "start"
                                    End If
                                End If
                                Dim zz As String = ""
                                x1 = CInt((x - ox) * sx)
                                y1 = CInt((y - oy) * sy)
                                If a Then zz = "transform='translate(" & x1 & "," & y1 & ") rotate(" & CStr(Int(360 - a)) & ") translate(-" & x1 & ",-" & y1 & ")'"
                                Response.Output.WriteLine("<text x='" & x1 & "' y='" & y1 & "' fill='" & fill & "'  " & zz & " font-size='" & CInt(z * sx) & "' font-family='Arial' text-anchor='" & ta & "' pointer-events='none'>" & svgEsc(label(C)) & "</text>")
                                x = x + z * si
                                y = y - z * co
                            Next
                        End If
                    End If
                End If
            End If
        Next

    End Sub


    Private Sub SelectedThemes(ByRef AvailTypes As Dictionary(Of String, Integer), ByVal VSET As String, ByVal PropertyNumber As String, ByVal viewNumber As String, ByVal colorsTop10 As List(Of colorTop10))
        Dim VQ As String = viewNumber
        Dim Q1, Q2 As Integer
        Dim TimeFrame As String = Session("TimeFrame")
        Dim viewtype As String
        Dim Connection As String = Session("Connection")
        Dim Lex As String = Session("LEX")
        ''Multiple views and legends for each property 
        TSQL = "SELECT " & Lex & "Themes.*, " & Lex & "SysData.* FROM " & Lex & "Themes LEFT JOIN " & Lex & "SysData ON " & Lex & "Themes.QNo=" & Lex & "SysData.QNo WHERE " & Lex & "Themes.VNo='" & CStr(viewNumber) & "' "
        Dim VTable As DataTable = GetTable(Connection, TSQL)
        If VTable.Rows.Count > 0 Then
            Dim VR As DataRow = VTable.Rows(0)
            If Not IsDBNull(VR("QNo")) Then VQ = VR("QNo")
            If Not IsDBNull(VR("QNo1")) Then Q1 = VR("QNo1")
            If Not IsDBNull(VR("QNo2")) Then Q2 = VR("QNo2")
            Dim ColorTable As String = VR("ColorTable") & ""
            Dim ColorField As String = ""
            If Len(ColorTable) Then ColorField = Trim(VR("QName") & "")

            If Len(ColorTable) Then ColorField = Trim(VR("QName") & "")
            Dim OptionCode As String = Trim(VR("OptionCode") & "")
            Dim VName As String = VR("VName")
            Dim CDataType As String = ""

            Dim CT As DataTable
            Dim CR As DataRow
            Dim FColors As Integer = 0
            'Get Custom QNo for M Theme
            'If Q1 > 0 And UCase(Trim(VR("VType") & "")) = "M" Then
            'Dim QNameRow() As DataRow = SystemTable.Select("QNo=" & Session("QNo"))
            '    Dim QName As String = QNameRow(0)("Name") & ""
            '    CDataType = QNameRow(0)("DataType") & ""
            '    VName = QName
            'End If
            ''''''''''''''''''''''''''''''''''''Store all selected Themes's View information for processing Stack and Drawing ''''''''''''''''''''''''''''''''''
            ' vlist.Add(New viewLists(viewNumber & "", viewtype, ColorTable, viewNumber, VR("Vacant"), Q1, Q2, OptionCode, ColorField))
            viewtype = Trim(UCase(VR("VType") & ""))
            Session("legendLists") = ""
            Select Case viewtype
                Case "B"
                    Session("tablewidth") = 960
                    CT = GetTable(Connection, "SELECT * FROM " & Lex & "AreaCategories WHERE BOMA=" & BOMA & " OR BOMA IS NULL ORDER BY CID")
                    Dim order As Integer = 0
                    If CT.Rows.Count > 0 Then
                        For Each CR In CT.Rows
                            Session("legendLists") &= "<td  align=" & Chr(34) & "center" & Chr(34) & " style=' margin-bottom:10px; margin-top:10px; margin-Left:5px; margin-right:5px;height:30px; text-align:center; background-color:" & HEXColor(CR("Color")) & ";' ><label class='labelView' style:'" & HEXColor(CR("Color")) & ";' runat='server'>" & Trim(CR("Description")) & "" & "</label></td>"
                            order += 1
                        Next
                    End If

                Case "C"
                    Dim Where As String = ""
                    TSQL = "SELECT Color, " & ColorField & " FROM " & Lex & ColorTable
                    CT = GetTable(Connection, TSQL)
                    If colorsTop10.Count > 0 Then
                        colorsTop10.Sort(Function(v1, v2) v1.RSF.CompareTo(v2.RSF))
                        For count As Integer = colorsTop10.Count - 1 To 0 Step -1
                            If count < colorsTop10.Count - 10 Then Exit For
                            Session("legendLists") &= "<td  align=" & Chr(34) & "center" & Chr(34) & " style=' margin-bottom:10px; margin-top:10px; margin-Left:5px; margin-right:5px;height:30px; text-align:center; background-color:" & colorsTop10(count).color & ";' ><label class='labelView' runat='server' style=' background-color:" & colorsTop10(count).color & ";' runat='server'>" & colorsTop10(count).description & "" & "</label></td>"
                            Session("tablewidth") = CInt(Session("tablewidth")) + 100
                        Next
                    End If
                    'Dim cview As String
                    'If CT.Rows.Count > 0 Then
                    '    Dim order As Integer = 0
                    '    For Each CR In CT.Rows
                    '        order += 1
                    '        If Not IsDBNull(CR("Color")) Then
                    '            cview = HEXColor(CR("Color"))
                    '        Else
                    '            cview = "FFFFFF"
                    '        End If
                    '        Session("legendLists") &= "<div  align=" & Chr(34) & "center" & Chr(34) & " style=' margin-bottom:10px; margin-top:10px; margin-Left:5px; margin-right:5px;height:30px; text-align:center; background-color:" & cview & ";' >" & CR(ColorField) & "" & "</div>"
                    '    Next
                    'End If

                Case "E"
                    CT = GetTable(Connection, "SELECT * FROM " & Lex & ColorTable & " ORDER BY Sort")
                    If CT.Rows.Count > 0 Then
                        Dim order As Integer = 0
                        For Each CR In CT.Rows
                            order += 1
                            Session("legendLists") &= "<td  align=" & Chr(34) & "center" & Chr(34) & "  style=' margin-bottom:10px; margin-top:10px; margin-Left:5px; margin-right:5px;height:30px; text-align:center; background-color:" & HEXColor(CR("Color")) & ";' ><label class='labelView' style:'" & HEXColor(CR("Color")) & ";' runat='server'>" & Trim(CR("Description")) & "" & "</label></td>"
                            Session("tablewidth") = CInt(Session("tablewidth")) + 100
                        Next
                    End If

                Case "F", "R"
                    TSQL = "SELECT * FROM " & Lex & "ThemeData WHERE (VNo=" & CStr(viewNumber) & ") AND (VSET='" & VSET & "')"
                    If viewtype = "R" Then TSQL = TSQL & " AND (UofM='" & UofM & "')"
                    CT = GetTable(Connection, TSQL)
                    If CT.Rows.Count Then
                        CR = CT.Rows(0)
                        Dim minval As Single = CDbl(CR("Base"))
                        Dim modval As Single = CDbl(CR("Step"))
                        Dim c As Integer = 0
                        Dim color As String = ""
                        If modval Then
                            For c = 1 To 9 Step 1
                                If Not IsDBNull(CR("O" & c & "")) Then
                                    color = HEXColor(CR("O" & c & "") & "")
                                Else
                                    color = HEXColor(ViewColor(c))
                                End If
                                Session("legendLists") &= "<td  align=" & Chr(34) & "center" & Chr(34) & "  style=' margin-bottom:10px; margin-top:10px; margin-Left:5px; margin-right:5px;height:30px; text-align:center; background-color:" & color & ";' ><label class='labelView' style:'" & color & ";' runat='server'>" & Trim(CR("C" & c & "") & "") & "" & "</label></td>"
                                Session("tablewidth") = CInt(Session("tablewidth")) + 100
                            Next
                        End If
                    End If

                Case "M"

                    Dim Where As String = ""
                    If colorsTop10.Count > 0 Then
                        Dim vcolor As Integer = 1
                        ' colorsTop10.Sort(Function(v1, v2) v1.RSF.CompareTo(v2.RSF))
                        For count As Integer = 0 To colorsTop10.Count - 1
                            Session("legendLists") &= "<td  align=" & Chr(34) & "center" & Chr(34) & " style=' margin-bottom:10px; margin-top:10px; margin-Left:5px; margin-right:5px;height:30px; text-align:center; background-color:" & HEXColor(ViewColor(vcolor)) & ";' ><label class='labelView' style:'" & HEXColor(ViewColor(vcolor)) & ";' runat='server'>" & colorsTop10(count).description & " (" & Format(colorsTop10(count).RSF, "#,###.#") & " " & UofA & ")</label></td>"
                            Session("tablewidth") = CInt(Session("tablewidth")) + 100
                            vcolor += 1
                        Next
                    End If

                Case "O"
                    Dim color As String = ""
                    If LCase(ColorTable) = "syscolors" Then
                        color = HEXColor(sysColor("Vacant", Connection, Lex))
                    Else
                        color = HEXColor(ViewColor(1))
                    End If
                    Session("legendLists") &= "<td  align=" & Chr(34) & "center" & Chr(34) & " style=' margin-bottom:10px; margin-top:10px; margin-Left:5px; margin-right:5px;height:30px; text-align:center; background-color:" & color & ";' ><label class='labelView' style:'" & color & ";' runat='server'>" & Trim(Resource("Portfolio", "Vacant", "Vacant")) & "" & "</label></td>"
                    Session("tablewidth") = CInt(Session("tablewidth")) + 100
                    If LCase(ColorTable) = "syscolors" Then
                        color = HEXColor(sysColor("Active", Connection, Lex))
                    Else
                        color = HEXColor(ViewColor(4))
                    End If
                    Session("legendLists") &= "<td  align=" & Chr(34) & "center" & Chr(34) & " style=' margin-bottom:10px; margin-top:10px; margin-Left:5px; margin-right:5px;height:30px; text-align:center; background-color:" & color & ";' ><label class='labelView' style:'" & color & ";' runat='server'>" & Trim(Resource("Portfolio", "Active", "Active")) & "" & "</label></td>"
                    Session("tablewidth") = CInt(Session("tablewidth")) + 100
                    If LCase(ColorTable) = "syscolors" Then
                        color = HEXColor(sysColor("Future", Connection, Lex))
                    Else
                        color = HEXColor(ViewColor(7))
                    End If
                    Session("tablewidth") = CInt(Session("tablewidth")) + 100
                    Session("legendLists") &= "<td style=' margin-bottom:10px; margin-top:10px; margin-Left:5px; margin-right:5px;height:30px; text-align:center; background-color:" & color & ";' ><label class='labelView' style:'" & color & ";' runat='server'>" & Trim(Resource("Portfolio", "Future", "Future")) & "" & "</label></td>"

                Case "V"
                    Dim color As String = ""
                    If LCase(ColorTable) = "syscolors" Then
                        color = HEXColor(sysColor("Vacant", Connection, Lex))
                    Else
                        color = HEXColor(ViewColor(1))
                    End If
                    Session("legendLists") &= "<td  align=" & Chr(34) & "center" & Chr(34) & " style=' margin-bottom:10px; margin-top:10px; margin-Left:5px; margin-right:5px;height:30px; text-align:center; background-color:" & color & ";' ><label class='labelView' style:'" & color & ";' runat='server'>" & Trim(Resource("Portfolio", "Vacant", "Vacant")) & "" & "</label></td>"
                    Session("tablewidth") = CInt(Session("tablewidth")) + 100
                    If LCase(ColorTable) = "syscolors" Then
                        color = HEXColor(sysColor("Active", Connection, Lex))
                    Else
                        color = HEXColor(ViewColor(4))
                    End If
                    Session("legendLists") &= "<td  align=" & Chr(34) & "center" & Chr(34) & "  style=' margin-bottom:10px; margin-top:10px; margin-Left:5px; margin-right:5px;height:30px; text-align:center; background-color:" & color & ";' ><label class='labelView' style:'" & color & ";' runat='server'>" & Trim(Resource("Portfolio", "Active", "Active")) & "" & "</label></td>"
                    Session("tablewidth") = CInt(Session("tablewidth")) + 100

                Case "L"
                    Dim modval As Single = 1
                    TSQL = "SELECT * FROM " & Lex & "ThemeData WHERE (VNo=" & CStr(viewNumber) & ") AND (VSET='" & VSET & "')"
                    CT = GetTable(Connection, TSQL)

                    If CT.Rows.Count > 0 Then
                        CR = CT.Rows(0)
                        If IsNumeric(CR("Step")) Then
                            modval = CInt(CR("Step"))
                        Else
                            modval = 1
                        End If
                        Dim Cviews As String = ""
                        Session("legendLists") &= "<td  align=" & Chr(34) & "center" & Chr(34) & " style=' margin-bottom:10px; margin-top:10px; margin-Left:5px; margin-right:5px;height:30px; text-align:center; background-color:" & HEXColor(ViewColor(1)) & ";' ><label class='labelView' style:'" & HEXColor(ViewColor(1)) & ";' runat='server'>" & Trim(Resource("Portfolio", "vacant", "vacant")) & "" & "</label></td>"
                        Session("tablewidth") = CInt(Session("tablewidth")) + 100
                        Session("legendLists") &= "<td  align=" & Chr(34) & "center" & Chr(34) & " style=' margin-bottom:10px; margin-top:10px; margin-Left:5px; margin-right:5px;height:30px; text-align:center; background-color:" & HEXColor(ViewColor(2)) & ";' ><label class='labelView' style:'" & HEXColor(ViewColor(2)) & ";' runat='server'>" & Trim(Resource("Portfolio", "MoToMo", "MoToMo")) & "" & "</label></td>"
                        Session("tablewidth") = CInt(Session("tablewidth")) + 100
                        Dim TViews As String = ""
                        Dim EViews As String = ""
                        Dim row As Integer = 3
                        For mvalue As Integer = 0 To 6 * modval Step modval
                            TViews = UCase(Format(DateAdd(DateInterval.Month, mvalue, CvDate(TimeFrame)), "MMM yyyy"))
                            EViews = UCase(Format(DateAdd(DateInterval.Month, mvalue + modval - 1, CvDate(TimeFrame)), "MMM yyyy"))
                            If TViews = EViews Then
                                EViews = ""
                            Else
                                EViews = " - " & EViews
                            End If
                            Dim category As String = ""
                            If mvalue = 6 * modval Then
                                category = Trim(CStr(TViews) & " +")
                            Else
                                category = Trim(CStr(TViews) & CStr(EViews))
                            End If
                            Session("legendLists") &= "<td  align=" & Chr(34) & "center" & Chr(34) & " style=' margin-bottom:10px; margin-top:10px; margin-Left:5px; margin-right:5px;height:30px; text-align:center; background-color:" & HEXColor(ViewColor(row)) & ";' ><label class='labelView' style:'" & HEXColor(ViewColor(row)) & ";' runat='server'>" & category & "" & "</label></td>"
                            Session("tablewidth") = CInt(Session("tablewidth")) + 100
                            row += 1
                        Next
                    End If

                Case "X", "W"
                    Dim CY As Integer
                    If viewtype = "X" Then
                        CY = DatePart("yyyy", DateAdd(DateInterval.Month, YearEnd, CvDate(TimeFrame)))
                    Else
                        CY = DatePart("yyyy", CvDate(TimeFrame))
                    End If
                    Dim Color As String = ""
                    If LCase(ColorTable) = "syscolors" Then
                        Color = HEXColor(sysColor("Vacant", Connection, Lex))
                    Else
                        Color = HEXColor(ViewColor(1))
                    End If
                    Session("legendLists") &= "<td  align=" & Chr(34) & "center" & Chr(34) & " style=' margin-bottom:10px; margin-top:10px; margin-Left:5px; margin-right:5px;height:30px; text-align:center; background-color:" & Color & ";' ><label class='labelView' style:'" & Color & ";' runat='server'>" & Trim(Resource("Portfolio", "vacant", "vacant")) & "" & "</label></td>"
                    Session("tablewidth") = CInt(Session("tablewidth")) + 100

                    If LCase(ColorTable) = "syscolors" Then
                        Color = HEXColor(sysColor("MoToMo", Connection, Lex))
                    Else
                        Color = HEXColor(ViewColor(2))
                    End If

                    Session("legendLists") &= "<td  align=" & Chr(34) & "center" & Chr(34) & " style=' margin-bottom:10px; margin-top:10px; margin-Left:5px; margin-right:5px;height:30px; text-align:center; background-color:" & Color & ";' ><label class='labelView' style:'" & Color & ";' runat='server'>" & Trim(Resource("Portfolio", "MoToMo", "MoToMo")) & "" & "</label></td>"
                    Session("tablewidth") = CInt(Session("tablewidth")) + 100
                    If LCase(ColorTable) = "syscolors" Then
                        Color = HEXColor(sysColor("Shade1", Connection, Lex))
                    Else
                        Color = HEXColor(ViewColor(3))
                    End If
                    Session("legendLists") &= "<td  align=" & Chr(34) & "center" & Chr(34) & " style=' margin-bottom:10px; margin-top:10px; margin-Left:5px; margin-right:5px;height:30px; text-align:center; background-color:" & Color & ";' ><label class='labelView' style:'" & Color & ";' runat='server'>" & Trim(CStr(CY)) & "" & "</label></td>"
                    Session("tablewidth") = CInt(Session("tablewidth")) + 100
                    If LCase(ColorTable) = "syscolors" Then
                        Color = HEXColor(sysColor("Shade2", Connection, Lex))
                    Else
                        Color = HEXColor(ViewColor(4))
                    End If
                    Session("legendLists") &= "<td  align=" & Chr(34) & "center" & Chr(34) & " style=' margin-bottom:10px; margin-top:10px; margin-Left:5px; margin-right:5px;height:30px; text-align:center; background-color:" & Color & ";' ><label class='labelView' style:'" & Color & ";' runat='server'>" & Trim(CStr(CY + 1)) & "" & "</label></td>"
                    Session("tablewidth") = CInt(Session("tablewidth")) + 100

                    If LCase(ColorTable) = "syscolors" Then
                        Color = HEXColor(sysColor("Shade3", Connection, Lex))
                    Else
                        Color = HEXColor(ViewColor(5))
                    End If
                    Session("legendLists") &= "<td  align=" & Chr(34) & "center" & Chr(34) & " style=' margin-bottom:10px; margin-top:10px; margin-Left:5px; margin-right:5px;height:30px; text-align:center; background-color:" & Color & ";' ><label class='labelView' style:'" & Color & ";' runat='server'>" & Trim(CStr(CY + 2)) & "" & "</label></td>"
                    Session("tablewidth") = CInt(Session("tablewidth")) + 100

                    If LCase(ColorTable) = "syscolors" Then
                        Color = HEXColor(sysColor("Shade4", Connection, Lex))
                    Else
                        Color = HEXColor(ViewColor(6))
                    End If
                    Session("legendLists") &= "<td  align=" & Chr(34) & "center" & Chr(34) & " style=' margin-bottom:10px; margin-top:10px; margin-Left:5px; margin-right:5px;height:30px; text-align:center; background-color:" & Color & ";' ><label class='labelView' style:'" & Color & ";' runat='server'>" & Trim(CStr(CY + 3)) & "" & "</label></td>"
                    Session("tablewidth") = CInt(Session("tablewidth")) + 100


                    If LCase(ColorTable) = "syscolors" Then
                        Color = HEXColor(sysColor("Shade5", Connection, Lex))
                    Else
                        Color = HEXColor(ViewColor(7))
                    End If
                    Session("legendLists") &= "<td  align=" & Chr(34) & "center" & Chr(34) & " style=' margin-bottom:10px; margin-top:10px; margin-Left:5px; margin-right:5px;height:30px; text-align:center; background-color:" & Color & ";' ><label class='labelView' style:'" & Color & ";' runat='server'>" & Trim(CStr(CY + 4)) & "" & "</label></td>"
                    Session("tablewidth") = CInt(Session("tablewidth")) + 100


                    If LCase(ColorTable) = "syscolors" Then
                        Color = HEXColor(sysColor("Shade6", Connection, Lex))
                    Else
                        Color = HEXColor(ViewColor(8))
                    End If
                    Session("legendLists") &= "<td  align=" & Chr(34) & "center" & Chr(34) & " style=' margin-bottom:10px; margin-top:10px; margin-Left:5px; margin-right:5px;height:30px; text-align:center; background-color:" & Color & ";' ><label class='labelView' style:'" & Color & ";' runat='server'>" & Trim(CStr(CY + 5)) & "" & "</label></td>"
                    Session("tablewidth") = CInt(Session("tablewidth")) + 100


                    If LCase(ColorTable) = "syscolors" Then
                        Color = HEXColor(sysColor("Shade7", Connection, Lex))
                    Else
                        Color = HEXColor(ViewColor(9))
                    End If
                    Session("legendLists") &= "<td  align=" & Chr(34) & "center" & Chr(34) & " style=' margin-bottom:10px; margin-top:10px; margin-Left:5px; margin-right:5px;height:30px; text-align:center; background-color:" & Color & ";' ><label class='labelView' style:'" & Color & ";' runat='server'>" & Trim(CStr(CY + 6)) & "" & "</label></td>"


                Case Else
            End Select
        End If


    End Sub


    Public Sub SVGTenantLogo()

        Dim i As Integer
        Dim currentX As Single, currentY As Single
        Dim lastX, lastY As Single
        Dim polyfill As Boolean = False
        Dim dx As Single, dy As Single, delta As Single
        Dim farc As Single, half As Single, radius As Single
        Dim ix1 As Long, iy1 As Long
        Dim icode As Long
        Dim stroke As String = "black"
        Dim fill As String = "none"
        Dim wall As Integer = 3 * sx / DXFScale
        Dim pi As Double = 4 * Math.Atan(1)

        Dim sb As New StringBuilder
        icode = 0
        For i = 1 To UBound(lg)
checkit:    If lg(i).W = -32768 Then GoTo nexti
            Select Case Math.Abs(lg(i).W)

                Case 0 To 299 ' line to
                    If sb.Length = 0 Then
                        sb.Append("<path fill='" & fill & "' stroke-width='3' stroke='" & stroke & "' d='M" & currentX & "," & currentY)
                    End If
                    ix1 = (lg(i).X - ox) * sx
                    iy1 = (lg(i).Y - oy) * sy
                    If icode = 1 Then
                        If (lg(i).X = lastX) And (lg(i).Y = lastY) Then
                        Else
                            sb.Append(" " & CStr(ix1) & "," & CStr(iy1))
                        End If
                    Else
                        sb.Append(" L" & CStr(ix1) & "," & CStr(iy1))
                        icode = 1
                    End If
                    lastX = lg(i).X
                    lastY = lg(i).Y
                    currentX = ix1
                    currentY = iy1

                Case 300 To 21599 ' arc to

                    dx = lg(i).X - lastX
                    dy = lg(i).Y - lastY
                    delta = Math.Sqrt(dx * dx + dy * dy)
                    farc = lg(i).W / (10800 / pi)
                    half = farc / 2
                    radius = Math.Abs(delta / (2 * Math.Sin(half)))
                    ix1 = (lg(i).X - ox) * sx
                    iy1 = (lg(i).Y - oy) * sy
                    If sb.Length = 0 Then
                        sb.Append("<path fill='" & fill & "' stroke-width='3' stroke='" & stroke & "' d='M" & currentX & "," & currentY)
                    End If
                    If lg(i).W < 0 Then
                        If CInt(radius * sx) > 10 Then
                            sb.Append(" A" & CInt(radius * sx) & "," & CInt(radius * sx) & " 0 0,1 " & CStr(ix1) & "," & CStr(iy1))
                            icode = 0
                        Else
                            sb.Append(" L" & CStr(ix1) & "," & CStr(iy1))
                            icode = 1
                        End If
                    Else
                        If CInt(radius * sx) > 10 Then
                            sb.Append(" A" & CInt(radius * sx) & "," & CInt(radius * sx) & " 0 0,0 " & CStr(ix1) & "," & CStr(iy1))
                            icode = 0
                        Else
                            sb.Append(" L" & CStr(ix1) & "," & CStr(iy1))
                            icode = 1
                        End If
                    End If
                    lastX = lg(i).X
                    lastY = lg(i).Y
                    currentX = ix1
                    currentY = iy1

                Case 21600 ' circle at

                    If sb.Length Then
                        sb.Append("'/>")
                        Response.Output.WriteLine(sb.ToString)
                        sb.Length = 0
                    End If
                    radius = Math.Abs(lg(i).X) * sx
                    sb.Append("<circle cx='" & currentX & "' cy='" & currentY & "' r='" & CStr(radius) & "'")
                    If lg(i).W < 0 Then
                        sb.Append(" fill='" & stroke & "' stroke-width='3' stroke='" & stroke & "' />")
                    Else
                        sb.Append(" fill='" & fill & "' stroke-width='3' stroke='" & stroke & "' />")
                    End If
                    Response.Output.WriteLine(sb.ToString)
                    sb.Length = 0
                    icode = 0


                Case 29000 'color and linewidth

                    If sb.Length Then
                        sb.Append("'/>")
                        Response.Output.WriteLine(sb.ToString)
                        sb.Length = 0
                    End If
                    Dim c As Integer = lg(i).X
                    If c = 256 Then c = Color.White.ToArgb
                    stroke = HEXColor(c)

                Case 30000 ' text



                Case 32000 ' fill

                    ix1 = (lg(i).X - ox) * sx
                    iy1 = (lg(i).Y - oy) * sy
                    If lg(i).W < 0 Then
                        sb.Append(" " & CStr(ix1) & "," & CStr(iy1) & " Z'/>")
                        Response.Output.WriteLine(sb.ToString)
                        sb.Length = 0
                        polyfill = False
                    Else
                        If sb.Length Then
                            sb.Append("'/>")
                            Response.Output.WriteLine(sb.ToString)
                            sb.Length = 0
                        End If
                        sb.Append("<path fill='" & stroke & "' stroke-width='3' stroke='" & stroke & "' d='M" & CStr(ix1) & "," & CStr(iy1))
                        polyfill = True
                    End If
                    lastX = lg(i).X
                    lastY = lg(i).Y
                    currentX = ix1
                    currentY = iy1
                    icode = 0

                Case 32767 ' move to (start a new line)
                    ix1 = (lg(i).X - ox) * sx
                    iy1 = (lg(i).Y - oy) * sy
                    If sb.Length Then
                        If (Not polyfill) And (sb.Length > 256) Then
                            sb.Append("'/>")
                            Response.Output.WriteLine(sb.ToString)
                            sb.Length = 0
                        Else
                            sb.Append(" M" & CStr(ix1) & "," & CStr(iy1))
                        End If
                    Else
                        sb.Append("<path fill='" & fill & "' stroke-width='3' stroke='" & stroke & "' d='M" & ix1 & "," & iy1)
                    End If
                    lastX = lg(i).X
                    lastY = lg(i).Y
                    currentX = ix1
                    currentY = iy1
                    icode = 0
            End Select
nexti:  Next i
        If sb.Length Then
            sb.Append("'/>")
            Response.Output.WriteLine(sb.ToString)
            sb.Length = 0
        End If

    End Sub

    Public Sub SVGBackground()

        Dim i As Integer
        Dim lastX, lastY As Single
        Dim currentX As Single, currentY As Single
        Dim polyfill As Integer, pi As Double
        Dim fontsize As Integer, rotation As Integer
        Dim textstring As String = ""
        Dim buffer As String = ""
        Dim dx As Single, dy As Single, delta As Single
        Dim farc As Single, half As Single, radius As Single
        Dim ix1 As Long, iy1 As Long
        Dim icode As Long
        Dim fill As String = "none"
        Dim stroke As String = "#000000"
        Dim layercolor As Integer
        Dim z As String

        polyfill = False
        Dim sb As New StringBuilder
        icode = 0
        pi = 4 * Math.Atan(1)
        For i = 1 To UBound(bg)
checkit:    If bg(i).W = -32768 Then
                If sb.Length Then
                    sb.Append("'/>")
                    Response.Output.WriteLine(sb.ToString)
                    sb.Length = 0
                End If
                Exit Sub
            End If
            Select Case Math.Abs(bg(i).W)
                Case 0 To 299 ' line to
                    If sb.Length = 0 Then
                        sb.Append("<path fill='" & fill & "' stroke='" & stroke & "' stroke-width='1' d='M" & currentX & "," & currentY)
                    End If
                    ix1 = (bg(i).X - ox) * sx
                    iy1 = (bg(i).Y - oy) * sy
                    If icode = 1 Then
                        If (bg(i).X = lastX) And (bg(i).Y = lastY) Then
                        Else
                            sb.Append(" " & CStr(ix1) & "," & CStr(iy1))
                        End If
                    Else
                        sb.Append(" L" & CStr(ix1) & "," & CStr(iy1))
                        icode = 1
                    End If
                    lastX = bg(i).X
                    lastY = bg(i).Y
                    currentX = ix1
                    currentY = iy1

                Case 300 To 21599 ' arc to

                    dx = bg(i).X - lastX
                    dy = bg(i).Y - lastY
                    delta = Math.Sqrt(dx * dx + dy * dy)
                    farc = bg(i).W / (10800 / pi)
                    half = farc / 2
                    radius = Math.Abs(delta / (2 * Math.Sin(half)))
                    ix1 = (bg(i).X - ox) * sx
                    iy1 = (bg(i).Y - oy) * sy
                    If sb.Length = 0 Then
                        sb.Append("<path fill='none' stroke='" & stroke & "' d='M" & currentX & "," & currentY)
                    End If
                    If bg(i).W < 0 Then
                        If CInt(radius * sx) > 10 Then
                            If bg(i).W < -10800 Then
                                sb.Append(" A" & CInt(radius * sx) & "," & CInt(radius * sx) & " 0 1,1 " & CStr(ix1) & "," & CStr(iy1))
                            Else
                                sb.Append(" A" & CInt(radius * sx) & "," & CInt(radius * sx) & " 0 0,1 " & CStr(ix1) & "," & CStr(iy1))
                            End If
                            icode = 0
                        Else
                            sb.Append(" L" & CStr(ix1) & "," & CStr(iy1))
                            icode = 1
                        End If
                    Else
                        If CInt(radius * sx) > 10 Then
                            If bg(i).W > 10800 Then
                                sb.Append(" A" & CInt(radius * sx) & "," & CInt(radius * sx) & " 0 1,0 " & CStr(ix1) & "," & CStr(iy1))
                            Else
                                sb.Append(" A" & CInt(radius * sx) & "," & CInt(radius * sx) & " 0 0,0 " & CStr(ix1) & "," & CStr(iy1))
                            End If
                            icode = 0
                        Else
                            sb.Append(" L" & CStr(ix1) & "," & CStr(iy1))
                            icode = 1
                        End If
                    End If

                    lastX = bg(i).X
                    lastY = bg(i).Y
                    currentX = ix1
                    currentY = iy1

                Case 21600 ' circle at
                        If sb.Length Then
                        sb.Append("'></path>")
                            Response.Output.WriteLine(sb.ToString)
                            sb.Length = 0
                        End If
                        radius = Math.Abs(bg(i).X) * sx
                        sb.Append("<circle cx='" & currentX & "' cy='" & currentY & "' r='" & CStr(radius) & "'")
                        If bg(i).W < 0 Then
                        sb.Append(" fill='none' stroke='" & stroke & "' />")
                        Else
                        sb.Append(" fill='none' stroke='" & stroke & "' />")
                        End If
                        Response.Output.WriteLine(sb.ToString)
                        sb.Length = 0
                        icode = 0

                Case 28000 To 28009 'linetype
                        'Dim pw As Single = bp.Width
                        'Select Case bg(i).W - 28000

                        '    Case 0 'Continuous
                        '        bp.DashStyle = DashStyle.Solid
                        '    Case 1 'Dashed        
                        '        bp.DashStyle = DashStyle.Custom
                        '        If pw Then
                        '            bp.DashPattern = New Single() {28 / pw, 8 / pw}
                        '        Else
                        '            bp.DashPattern = New Single() {28, 8}
                        '        End If
                        '    Case 2 'Hidden
                        '        bp.DashStyle = DashStyle.Custom
                        '        If pw Then
                        '            bp.DashPattern = New Single() {14 / pw, 4 / pw}
                        '        Else
                        '            bp.DashPattern = New Single() {14, 4}
                        '        End If
                        '    Case 3 'Hidden2
                        '        bp.DashStyle = DashStyle.Custom
                        '        If pw Then
                        '            bp.DashPattern = New Single() {7 / pw, 2 / pw}
                        '        Else
                        '            bp.DashPattern = New Single() {7, 2}
                        '        End If
                        '    Case 4 'Center         
                        '        bp.DashStyle = DashStyle.Custom
                        '        If pw Then
                        '            bp.DashPattern = New Single() {64 / pw, 8 / pw, 12 / pw, 8 / pw}
                        '        Else
                        '            bp.DashPattern = New Single() {64, 8, 12, 8}
                        '        End If
                        '    Case 5 'Center2
                        '        bp.DashStyle = DashStyle.Custom
                        '        If pw Then
                        '            bp.DashPattern = New Single() {32 / pw, 4 / pw, 6 / pw, 4 / pw}
                        '        Else
                        '            bp.DashPattern = New Single() {32, 4, 6, 4}
                        '        End If
                        '    Case 6 'DashDot
                        '        bp.DashStyle = DashStyle.Custom
                        '        If pw Then
                        '            bp.DashPattern = New Single() {24 / pw, 6 / pw, 2 / pw, 6 / pw}
                        '        Else
                        '            bp.DashPattern = New Single() {24, 6, 2, 6}
                        '        End If
                        '    Case 7 'Phantom             
                        '        bp.DashStyle = DashStyle.Custom
                        '        If pw Then
                        '            bp.DashPattern = New Single() {80 / pw, 12 / pw, 14 / pw, 12 / pw, 14 / pw, 12 / pw}
                        '        Else
                        '            bp.DashPattern = New Single() {80, 12, 14, 12, 14, 12}
                        '        End If
                        '    Case 8 'Phantom2
                        '        bp.DashStyle = DashStyle.Custom
                        '        If pw Then
                        '            bp.DashPattern = New Single() {40 / pw, 6 / pw, 7 / pw, 6 / pw, 7 / pw, 6 / pw}
                        '        Else
                        '            bp.DashPattern = New Single() {40, 6, 7, 6, 7, 6}
                        '        End If
                        '    Case 9 'Divide
                        '        bp.DashStyle = DashStyle.Custom
                        '        If pw Then
                        '            bp.DashPattern = New Single() {24 / pw, 6 / pw, 2 / pw, 6 / pw, 2 / pw, 6 / pw}
                        '        Else
                        '            bp.DashPattern = New Single() {24, 6, 2, 6, 2, 6}
                        '        End If
                        'End Select


                Case 29000 'color and linewidth
                    If sb.Length Then
                        sb.Append("'/>")
                        Response.Output.WriteLine(sb.ToString)
                        sb.Length = 0
                    End If
                        Dim Width As Integer = Math.Abs(bg(i).Y)
                        If bg(i).X = -1 Then
                        'If bg(i).Y = 0 Then
                        '    Dim c As Integer = bg(i).X
                        '    If c = 256 Then
                        '        c = Color.White.ToArgb
                        '        stroke = HEXColor(c)
                        '    End If
                        'End If
                        Else
                        layercolor = bg(i).X
                        If layercolor = 256 Then layercolor = Color.White.ToArgb
                        stroke = HEXColor(layercolor)
                        End If

                Case 29001 'Symbol Color
                    Dim c As Integer = bg(i).X
                    If c = 256 Then c = Color.White.ToArgb
                    stroke = HEXColor(c)

                Case 29002 'Symbol Color Reset
                    Dim c As Integer = layercolor
                    If c = 256 Then c = Color.White.ToArgb
                    stroke = HEXColor(c)

                Case 30000 ' text
                        If sb.Length Then
                            sb.Append("'/>")
                            Response.Output.WriteLine(sb.ToString)
                            sb.Length = 0
                        End If
                        If bg(i).W < 0 Then
                            textstring = ""
                            rotation = bg(i).X
                            fontsize = bg(i).Y * Math.Abs(sy) * 1.3
                            i = i + 1
                        End If
                        Do While bg(i).W = 30000
                            textstring = textstring & PXFtext(bg(i).X) & PXFtext(bg(i).Y)
                            i = i + 1
                        Loop
                        If Customary And 256 Then
                            If rotation = 0 Then
                                z = ""
                            Else
                                z = "transform='translate(" & currentX & "," & currentY & ") rotate(" & CStr(360 - rotation) & ") translate(-" & currentX & ",-" & currentY & ")'"
                            End If
                        '  Response.Output.WriteLine("<text style='fill:" & stroke & ";font-size:" & fontsize & ";' x='" & currentX & "' y='" & currentY & "' " & z & " pointer-events='none'>" & svgEsc(Trim(textstring)) & "</text>")
                        Response.Output.WriteLine("<text fill='" & stroke & "' font-family='Arial' font-size='" & fontsize & "' x='" & currentX & "' y='" & currentY & "' " & z & " pointer-events='none'>" & svgEsc(Trim(textstring)) & "</text>")
                        End If
                        icode = 0
                        GoTo checkit

                Case 30001 ' text
                        If sb.Length Then
                            sb.Append("'/>")
                            Response.Output.WriteLine(sb.ToString)
                            sb.Length = 0
                        End If
                        If bg(i).W < 0 Then
                            textstring = ""
                            rotation = bg(i).X
                            fontsize = bg(i).Y * Math.Abs(sy) * 1.3
                            i = i + 1
                        End If
                        Do While bg(i).W = 30001
                            textstring = textstring & DBPXFtext(bg(i).X) & DBPXFtext(bg(i).Y)
                            i = i + 1
                        Loop
                        If Customary And 256 Then
                            If rotation = 0 Then
                                z = ""
                            Else
                                z = "transform='translate(" & currentX & "," & currentY & ") rotate(" & CStr(360 - rotation) & ") translate(-" & currentX & ",-" & currentY & ")'"
                            End If
                        Response.Output.WriteLine("<text fill='" & stroke & "' font-family='Arial' font-size='" & fontsize & "' x='" & currentX & "' y='" & currentY & "' " & z & " pointer-events='none'>" & svgEsc(Trim(textstring)) & "</text>")
                        End If
                        icode = 0
                        GoTo checkit

                Case 32000 ' fill
                        ix1 = (bg(i).X - ox) * sx
                        iy1 = (bg(i).Y - oy) * sy
                        If bg(i).W < 0 Then
                            sb.Append(" " & CStr(ix1) & "," & CStr(iy1) & " Z'/>")
                            Response.Output.WriteLine(sb.ToString)
                            sb.Length = 0
                            polyfill = False
                        Else
                            If sb.Length Then
                            sb.Append("'></path>")
                                Response.Output.WriteLine(sb.ToString)
                                sb.Length = 0
                            End If
                        sb.Append("<path ' fill='" & stroke & "' stroke='" & stroke & "' stroke-width='1'  d='M" & CStr(ix1) & "," & CStr(iy1))
                            polyfill = True
                        End If
                        lastX = bg(i).X
                        lastY = bg(i).Y
                        currentX = ix1
                        currentY = iy1
                        icode = 0

                Case 32767 ' move to (start a new line)
                        ix1 = (bg(i).X - ox) * sx
                        iy1 = (bg(i).Y - oy) * sy
                        If sb.Length Then
                            If (Not polyfill) And (sb.Length > 256) Then
                            sb.Append("' ></path>")
                                Response.Output.WriteLine(sb.ToString)
                                sb.Length = 0
                            Else
                                sb.Append(" M" & CStr(ix1) & "," & CStr(iy1))
                            End If
                        Else
                        sb.Append("<path fill='" & fill & "' stroke='" & stroke & "' stroke-width='1'  d='M" & ix1 & "," & iy1)
                        End If
                        lastX = bg(i).X
                        lastY = bg(i).Y
                        currentX = ix1
                        currentY = iy1
                        icode = 0
            End Select
        Next i
        If sb.Length Then
            sb.Append("'/>")
            Response.Output.WriteLine(sb.ToString)
            sb.Length = 0
        End If
    End Sub

    Public Shared Function PXFtext(ByVal xy As Single) As String
        Dim s(3) As Byte
        s = BitConverter.GetBytes(xy)
        Dim sb As New StringBuilder(4)
        Dim j As Integer
        For j = 0 To 3
            If s(j) < 32 Then s(j) = 32
            sb.Append(Chr(s(j)))
        Next
        PXFtext = sb.ToString
    End Function


    Public Shared Function DBPXFtext(ByVal xy As Single) As String
        Dim s(3) As Byte
        s = BitConverter.GetBytes(xy)
        Dim sb As New StringBuilder
        sb.Append(ChrW(BitConverter.ToInt16(s, 0)))
        sb.Append(ChrW(BitConverter.ToInt16(s, 2)))
        DBPXFtext = sb.ToString
    End Function


    Function IpadViews(ByVal detailsList As List(Of detailsLists), ByVal j As Integer, ByVal g As Integer, ByVal n As Integer, ByVal Viewset As String, ByVal PropertyNumber As String, ByRef cviewTable As Dictionary(Of String, Integer), ByRef colorTop10s As List(Of colorTop10))
        Dim fill As String = "#FFFFFF"
        Dim k As Integer = 0
        Dim FColors As Integer
        Dim TimeFrame As String = Session("TimeFrame")
        Dim DR As DataRow
        Dim DT As DataTable
        ViewColor(1) = Color.Red.ToArgb
        ViewColor(2) = Color.Orange.ToArgb
        ViewColor(3) = Color.Yellow.ToArgb
        ViewColor(4) = Color.SpringGreen.ToArgb
        ViewColor(5) = Color.Cyan.ToArgb
        ViewColor(6) = Color.DeepSkyBlue.ToArgb
        ViewColor(7) = Color.Violet.ToArgb
        ViewColor(8) = Color.Plum.ToArgb
        ViewColor(9) = Color.Salmon.ToArgb
        If Len(Trim(YearEnd)) > 0 Then YearEnd = Convert.ToInt32(Session("YearEnd"))
        If Len(Viewset) Then
            TSQL = "SELECT C1, C2, C3, C4, C5, C6, C7,C8,C9 FROM Viewsets WHERE VSET='" & Viewset & "'"
            Dim VT As DataTable = GetTable(Session("Connection"), TSQL)
            If VT.Rows.Count Then
                Dim VR As DataRow = VT.Rows(0)
                If Not IsDBNull(VR("C1")) Then ViewColor(1) = VR("C1")
                If Not IsDBNull(VR("C2")) Then ViewColor(2) = VR("C2")
                If Not IsDBNull(VR("C3")) Then ViewColor(3) = VR("C3")
                If Not IsDBNull(VR("C4")) Then ViewColor(4) = VR("C4")
                If Not IsDBNull(VR("C5")) Then ViewColor(5) = VR("C5")
                If Not IsDBNull(VR("C6")) Then ViewColor(6) = VR("C6")
                If Not IsDBNull(VR("C7")) Then ViewColor(7) = VR("C7")
                If Not IsDBNull(VR("C8")) Then ViewColor(8) = VR("C8")
                If Not IsDBNull(VR("C9")) Then ViewColor(9) = VR("C9")
            End If
        End If

        For i = 1 To UBound(fg)
            If (fg(i).C1 And 15) = 9 Then fg(i).P2 = (fg(i).P2 And -256)
            'If (fg(i).C1 And 15) = 12 Then fg(i).P2 = (fg(i).P2 And -256) 'Custom Text and Stack
        Next

        Dim Lex As String = Session("LEX")
        Dim Connection As String = Session("Connection")
        Dim ViewType As String
        TSQL = "SELECT " & Lex & "Themes.*, " & Lex & "SysData.* FROM " & Lex & "Themes LEFT JOIN " & Lex & "SysData ON " & Lex & "Themes.QNo=" & Lex & "SysData.QNo WHERE " & Lex & "Themes.VNo=" & CStr(ViewNumber)
        DT = GetTable(Connection, TSQL)
        DR = DT.Rows(0)
        Dim Vacant As Boolean = DR("Vacant")
        ViewType = Trim(DR("VType") & "")
        Dim VQ, Q1, Q2 As Integer
        Dim OptionCode As String = Trim(DR("OptionCode") & "")
        If Not IsDBNull(DR("QNo")) Then VQ = DR("QNo")
        If Not IsDBNull(DR("QNo1")) Then Q1 = DR("QNo1")
        If Not IsDBNull(DR("QNo2")) Then Q2 = DR("QNo2")
        Dim TotalAmount As Double = 0.0
        Dim CDataType As String = ""
        Dim ColorTable As String = DR("ColorTable") & ""
        Dim ColorField As String = ""
        If Len(ColorTable) Then
            ColorField = Trim(DR("QName") & "")
        End If
        If Not IsDBNull(DR("QNo")) Then VQ = DR("QNo")

        If Len(Trim(ViewType)) = 0 Then
            fill = "#C0C0C0"
        Else
            If Trim(ViewType) = "M" Then
                'If Q1 > 0 Then
                '    VQ = DR("QNo")
                '    TSQL = "SELECT field,Name,DataType FROM " & Lex & "SysData WHERE QNo=" & VQ & ""
                '    Dim QT As DataTable = GetTable(Connection, TSQL)
                '    Dim QName As String = QT(0)("Name") & ""
                '    CDataType = QT(0)("DataType") & ""
                '    If UCase(Trim(CDataType)) = "RENT" Then QName = QName & " " & PER
                'End If
                k = 255
                If Vacant = True And Len(detailsList(j).LNo) Then GoTo Next_M1
                If Len(detailsList(j).VQNo) Then
                    fill = "#FFFFFF"
                    If colorTop10s.Count > 0 Then
                        For m = 0 To colorTop10s.Count - 1
                            If Trim(CStr(detailsList(j).VQNo)) = Trim(CStr(CStr(colorTop10s(m).description))) Then
                                fill = HEXColor(ViewColor(m + 1))
                                GoTo Next_M1
                            End If
                        Next
                        fill = "#C0C0C0"
                    End If
                Else
                    fill = "#FFFFFF"
Next_M1:        End If
            End If


            ''''''''''''''''''''''''''''''''' Color Table''''''''''''''''''''''''''''''''''''''''''''''''
            If Trim(ViewType) = "C" Then
                TSQL = "SELECT * FROM " & Lex & "ThemeData WHERE (VNo=" & CStr(ViewNumber) & ") AND (VSET='" & Viewset & "')"
                DT = GetTable(Connection, TSQL)
                If DT.Rows.Count Then
                    DR = DT.Rows(0)
                    If IsDBNull(DR("Base")) Then
                        FColors = 0
                    Else
                        FColors = DR("Base")
                    End If
                    If FColors Then
                        For i = 1 To FColors
                            If Not IsDBNull(DR("O" & CStr(i))) Then
                                cviewTable.Add(DR("C" & CStr(i)), DR("O" & CStr(i)))
                            Else
                                cviewTable.Add(DR("C" & CStr(i)) & "", ViewColor(i))
                            End If
                        Next
                    End If

                    If Vacant = True And Len(detailsList(j).LNo) Then GoTo Next_C1

                    If Len(detailsList(j).VQNo) Then
                        If cviewTable.Count > 0 Then
                            For Each de As KeyValuePair(Of String, Integer) In cviewTable
                                If CStr(detailsList(j).VQNo) = CStr(CStr(de.Key)) Then
                                    GoTo Next_C1
                                End If
                            Next
                        End If
Next_C1:            End If
                    Dim where As String = ""
                    If OptionCode = "P" Then where = " WHERE PNo='" & PropertyNumber & "'"
                    TSQL = "SELECT  " & ColorField & ",Color FROM " & Lex & ColorTable & where & " Group by " & ColorField & ", Color Order by " & ColorField
                    Dim CT As DataTable = GetTable(Connection, TSQL)
                    Dim copyCviewTable As New Dictionary(Of String, Integer)
                    Dim flag As Boolean = False
                    If CT.Rows.Count > 0 Then
                        cviewTable = New Dictionary(Of String, Integer)
                        Dim col As Integer = 0
                        For Each CR As DataRow In CT.Rows
                            If Not IsDBNull(CR("Color")) Then
                                col = CR("Color")
                            Else
                                col = -4144960
                            End If
                            If Trim(CStr(detailsList(j).VQNo)) = Trim(CStr(CR(ColorField))) Then
                                cviewTable.Add(CR(ColorField), col)
                                Exit For
                            End If
                        Next
                    Else
                        Dim count As Integer = 0
                        Dim keys As List(Of String) = cviewTable.Keys.ToList
                        flag = True
                        keys.Sort()
                        For Each de As KeyValuePair(Of String, Integer) In cviewTable
                            If count < 9 Then
                                For Each Str As String In keys
                                    If Str = de.Key Then
                                        copyCviewTable.Add(Str, count + 1)
                                        count += 1
                                    End If
                                Next
                            Else
                            End If
                        Next

                    End If
                    If flag = True Then
                        cviewTable = New Dictionary(Of String, Integer)
                        cviewTable = copyCviewTable
                    End If
                    If Vacant = True And Len(detailsList(j).LNo) Then GoTo Next_C5
                    If cviewTable.Count > 1 Then
                        For Each de As KeyValuePair(Of String, Integer) In cviewTable
                            If Trim(CStr(detailsList(j).VQNo)) = Trim(CStr(de.Key)) Then
                                If flag = True Then
                                    k = ViewColor(de.Value)
                                Else
                                    k = de.Value
                                End If
                                Session("ID") = ""
                                If colorTop10s.Count > 0 Then
                                    Dim colorTop As colorTop10
                                    Session("ID") = detailsList(j).VQNo
                                    colorTop = colorTop10s.Find(AddressOf findCategories)

                                    If colorTop IsNot Nothing Then

                                        colorTop10s.Find(Function(t As colorTop10) t.description = detailsList(j).VQNo).RSF += detailsList(j).Rentable
                                    Else
                                        colorTop10s.Add(New colorTop10(detailsList(j).UNo, HEXColor(k), detailsList(j).VQNo, detailsList(j).Rentable))
                                    End If
                                Else
                                    colorTop10s.Add(New colorTop10(detailsList(j).UNo, HEXColor(k), detailsList(j).VQNo, detailsList(j).Rentable))
                                End If
                                GoTo Next_C5
                            End If
                        Next
                    End If
Next_C5:            If k = 0 Then
                        fill = "#FFFFFF"
                    Else
                        fill = HEXColor(ViewColor(k))
                    End If
                End If
            End If


            '''''''''''''''''''''''AreaCategories''''''''''''''''''''''''''''''''''
            If Trim(ViewType) = "B" Then ''Area Categories
                Dim CT As DataTable = GetTable(Connection, "SELECT * FROM " & Lex & "AreaCategories WHERE BOMA=" & BOMA & " OR BOMA IS NULL ORDER BY CID")
                Dim AreaCategories(CT.Rows.Count) As Integer
                If CT.Rows.Count > 0 Then
                    Dim a As Integer = 1
                    For Each Row As DataRow In CT.Rows
                        AreaCategories(a) = Row("Color")
                        a += 1
                    Next
                End If
                n = rg(g).ARec
                k = fg(n).P1 And 7
                If k = 0 Then
                    fill = "#FFFFFF"
                Else
                    ''Add color to fill table 
                    fill = HEXColor(AreaCategories(k))
                End If
            End If

            '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
            If Trim(ViewType) = "E" Then

                Dim Legend As DataTable = GetTable(Connection, "SELECT * FROM " & Lex & ColorTable & " ORDER BY Sort")
                Dim VacRow, count As Integer
                Dim ActRow As Integer
                Dim FutRow As Integer
                Dim vcolor As New Hashtable()
                Dim color(Legend.Rows.Count) As Integer
                count = 1
                For Each vie As DataRow In Legend.Rows
                    If vie("Expiration") = "0" Then
                        VacRow = count
                    ElseIf vie("Expiration") = "1" Then
                        ActRow = count
                    ElseIf vie("Expiration") = "2" Then
                        FutRow = count
                    End If
                    '' Add future Description for Year calculation
                    vcolor.Add(count, vie("Description"))
                    ''Store color from color table
                    color(count) = vie("Color")
                    count += 1
                Next
                Dim RSF As Integer
                k = 255
                fill = "#FFFFFF"
                RSF = detailsList(j).Rentable
                If RSF Then
                    If Len(detailsList(j).Q1) Then
                        If detailsList(j).Q1 > CvDate(TimeFrame) Then 'future
                            k = FutRow
                        Else
                            If Len(detailsList(j).Q2) Then
                                Dim year As String = DatePart(DateInterval.Year, detailsList(j).Q2).ToString
                                ''Check year same as Description from Expiry Table
                                For Each de As DictionaryEntry In vcolor
                                    If de.Value = year Then k = de.Key
                                Next
                                If k = 255 Then k = ActRow
                            Else 'month to month
                                k = ActRow
                            End If
                        End If
                    Else 'vacant
                        k = VacRow
                    End If
                End If
                If k = 0 Or k = 255 Then
                    fill = "#FFFFFF"
                Else
                    ''Add color to fill table 
                    If Len(Trim(ColorTable)) > 0 Then
                        fill = HEXColor(color(k))
                    End If
                End If

            End If

            ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
            If Trim(ViewType) = "O" Then
                fill = "#FFFFFF"
                k = 0
                If Len(Trim(detailsList(j).VQNo)) > 0 Then
                    If detailsList(j).VQNo > CvDate(TimeFrame) Then
                        k = 3
                    Else
                        k = 2
                    End If
                Else
                    k = 1
                End If
                If k > 0 Or k < 255 Then
                    If Len(Trim(ColorTable)) > 0 Then
                        If k = 1 Then fill = HEXColor(sysColor("Vacant", Session("Connection"), Session("LEX")))
                        If k = 2 Then fill = HEXColor(sysColor("Active", Session("Connection"), Session("LEX")))
                        If k = 3 Then fill = HEXColor(sysColor("Future", Session("Connection"), Session("LEX")))
                    Else
                        If k = 1 Then
                            fill = HEXColor(ViewColor(1))
                        ElseIf k = 2 Then
                            fill = HEXColor(ViewColor(4))
                        ElseIf k = 3 Then
                            fill = HEXColor(ViewColor(7))
                        End If
                    End If
                Else
                    fill = "#FFFFFF"
                End If

            End If

            ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
            If Trim(ViewType) = "V" Then

                k = 255
                fill = "#FFFFFF"

                If Len(detailsList(j).VQNo) Then
                    If detailsList(j).VQNo <= CvDate(TimeFrame) Then
                        k = 2
                    Else
                        k = 1
                    End If
                Else
                    k = 1
                End If

                If k > 0 Or k < 255 Then
                    If Len(Trim(ColorTable)) > 0 Then
                        If k = 1 Then fill = HEXColor(sysColor("Vacant", Session("Connection"), Session("LEX")))
                        If k = 2 Then fill = HEXColor(sysColor("Active", Session("Connection"), Session("LEX")))
                    Else
                        If k = 1 Then
                            fill = HEXColor(ViewColor(1))
                        ElseIf k = 2 Then
                            fill = HEXColor(ViewColor(4))
                        End If
                    End If
                Else
                    fill = "#FFFFFF"
                End If
            End If

            '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
            Dim modval As Single
            If Trim(ViewType) = "L" Then
                fill = "#FFFFFF"
                modval = 1
                TSQL = "SELECT * FROM " & Lex & "ThemeData WHERE (VNo=" & CStr(ViewNumber) & ") AND (VSET='" & Viewset & "')"
                DT = GetTable(Connection, TSQL)

                If DT.Rows.Count > 0 Then
                    DR = DT.Rows(0)
                    If IsNumeric(DR("Step")) Then
                        modval = CInt(DR("Step"))
                    Else
                        modval = 1
                    End If
                    k = 255
                    Dim TViews As String = ""
                    Dim Cviews As String = Format(DateAdd(DateInterval.Month, 0, CvDate(TimeFrame)), "MM-yyyy")
                    If Len(detailsList(j).LNo) Then
                        If Len(CStr(detailsList(j).VQNo)) Then
                            TViews = Format(detailsList(j).VQNo, "MMM yyyy")
                            If CDate(TViews) < CDate(Cviews) Or detailsList(j).VQNo < CvDate(TimeFrame) Then
                                k = 2
                            ElseIf CDate(TViews) >= CDate(Cviews) And CDate(TViews) < CDate(Format(DateAdd(DateInterval.Month, modval, CvDate(TimeFrame)), "MM yyyy")) Then
                                k = 3
                            ElseIf CDate(TViews) >= CDate(Format(DateAdd(DateInterval.Month, modval * 1, CvDate(TimeFrame)), "MM yyyy")) And CDate(TViews) < CDate(Format(DateAdd(DateInterval.Month, modval * 2, CvDate(TimeFrame)), "MM yyyy")) Then
                                k = 4
                            ElseIf CDate(TViews) >= CDate(Format(DateAdd(DateInterval.Month, modval * 2, CvDate(TimeFrame)), "MM yyyy")) And CDate(TViews) < CDate(Format(DateAdd(DateInterval.Month, modval * 3, CvDate(TimeFrame)), "MM yyyy")) Then
                                k = 5
                            ElseIf CDate(TViews) >= CDate(Format(DateAdd(DateInterval.Month, modval * 3, CvDate(TimeFrame)), "MM yyyy")) And CDate(TViews) < CDate(Format(DateAdd(DateInterval.Month, modval * 4, CvDate(TimeFrame)), "MM yyyy")) Then
                                k = 6
                            ElseIf CDate(TViews) >= CDate(Format(DateAdd(DateInterval.Month, modval * 4, CvDate(TimeFrame)), "MM yyyy")) And CDate(TViews) < CDate(Format(DateAdd(DateInterval.Month, modval * 5, CvDate(TimeFrame)), "MM yyyy")) Then
                                k = 7
                            ElseIf CDate(TViews) >= CDate(Format(DateAdd(DateInterval.Month, modval * 5, CvDate(TimeFrame)), "MM yyyy")) And CDate(TViews) < CDate(Format(DateAdd(DateInterval.Month, modval * 6, CvDate(TimeFrame)), "MM yyyy")) Then
                                k = 8
                            ElseIf CDate(TViews) >= CDate(Format(DateAdd(DateInterval.Month, modval * 6, CvDate(TimeFrame)), "MM yyyy")) Then
                                k = 9
                            Else
                                k = 0
                            End If
                        Else
                            k = 2
                        End If
                    Else
                        k = 1
                    End If
                    fill = HEXColor(ViewColor(k))
                End If
            End If
            ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
            If Trim(ViewType) = "W" Or Trim(ViewType) = "X" Then
                fill = "#FFFFFF"
                If Len(detailsList(j).LNo) Then
                    Dim CY As Integer = DatePart("yyyy", CvDate(TimeFrame))
                    If Len(CStr(detailsList(j).VQNo)) Then

                        Dim TY As Integer
                        If ViewType = "X" Then
                            TY = DatePart("yyyy", DateAdd(DateInterval.Month, YearEnd, detailsList(j).VQNo))
                        Else
                            TY = DatePart("yyyy", detailsList(j).VQNo)
                        End If
                        If TY < CY Or detailsList(j).VQNo < CvDate(TimeFrame) Then
                            k = 2
                        ElseIf TY = CY Then
                            k = 3
                        ElseIf TY = CY + 1 Then
                            k = 4
                        ElseIf TY = CY + 2 Then
                            k = 5
                        ElseIf TY = CY + 3 Then
                            k = 6
                        ElseIf TY = CY + 4 Then
                            k = 7
                        ElseIf TY = CY + 5 Then
                            k = 8
                        Else
                            k = 9
                        End If
                    Else
                        k = 2
                    End If
                Else
                    k = 1
                End If
                If Len(Trim(ColorTable)) > 0 Then
                    fill = loadsyscolors(k)
                Else
                    fill = HEXColor(ViewColor(k))
                End If

            End If

            ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
            If Trim(ViewType) = "F" Or Trim(ViewType) = "R" Then
                fill = "#FFFFFF"
                TSQL = "SELECT * FROM " & Lex & "ThemeData WHERE (VNo=" & ViewNumber & ") AND (VSET='" & Viewset & "')"
                If Trim(ViewType) = "R" Then TSQL = TSQL & " AND (UofM='" & UofM & "')"
                Dim TD As DataTable = GetTable(Connection, TSQL)
                If TD.Rows.Count > 0 Then
                    Dim row As DataRow = TD.Rows(0)
                    Dim View(10) As Integer
                    ''''Assigning array update the original Viewcolor with new View. So we need to copy the array.
                    System.Array.Copy(ViewColor, View, 10)
                    If Not IsDBNull(row("O1")) Then View(1) = row("O1")
                    If Not IsDBNull(row("O2")) Then View(2) = row("O2")
                    If Not IsDBNull(row("O3")) Then View(3) = row("O3")
                    If Not IsDBNull(row("O4")) Then View(4) = row("O4")
                    If Not IsDBNull(row("O5")) Then View(5) = row("O5")
                    If Not IsDBNull(row("O6")) Then View(6) = row("O6")
                    If Not IsDBNull(row("O7")) Then View(7) = row("O7")
                    If Not IsDBNull(row("O8")) Then View(8) = row("O8")
                    If Not IsDBNull(row("O9")) Then View(9) = row("O9")
                    Dim minval As Single = 0
                    modval = 1
                    Dim maxval As Single
                    Dim rowval As Single
                    Try
                        minval = CDbl(row("Base"))
                        modval = CDbl(row("Step"))
                        If InStr(row("Step"), ".") = 0 Then
                            maxval = 0.9999
                        Else
                            maxval = 0.9999 * 10 ^ -(Len(row("Step")) - InStr(row("Step"), "."))
                        End If
                    Catch
                        MsgBox(Resource("Folder", "Msg12", "Step value must be greater than zero"))
                    End Try
                    k = 0
                    ' fill = "white"
                    If Vacant = True And Len(detailsList(j).LNo) Then GoTo Next_F2
                    If Len(CStr(detailsList(j).VQNo)) Then
                        rowval = Val(detailsList(j).VQNo)                          '                            If Value Then
                        k = Fix((rowval - minval - maxval) / modval) + 1
                        If k < 1 Then k = 1
                        If k > 9 Then k = 9
                    End If
Next_F2:            If (k = 255 Or k = 0) Then
                        fill = "#FFFFFF"
                    Else
                        fill = HEXColor(View(k))
                    End If
                End If
            End If
            End If
            Return fill
    End Function

    Private Function findCategories(ByVal colorTop As colorTop10) As Boolean
        If colorTop.description = Session("Id") Then
            Return True
        Else
            Return False
        End If

    End Function

    Private Function loadsyscolors(ByVal k As Integer) As String
        Select Case k
            Case 1
                loadsyscolors = HEXColor(sysColor("Vacant", Session("Connection"), Session("LEX")))
            Case 2
                loadsyscolors = HEXColor(sysColor("MoToMo", Session("Connection"), Session("LEX")))
            Case 3
                loadsyscolors = HEXColor(sysColor("Shade1", Session("Connection"), Session("LEX")))
            Case 4
                loadsyscolors = HEXColor(sysColor("Shade2", Session("Connection"), Session("LEX")))
            Case 5
                loadsyscolors = HEXColor(sysColor("Shade3", Session("Connection"), Session("LEX")))
            Case 6
                loadsyscolors = HEXColor(sysColor("Shade4", Session("Connection"), Session("LEX")))
            Case 7
                loadsyscolors = HEXColor(sysColor("Shade5", Session("Connection"), Session("LEX")))
            Case 8
                loadsyscolors = HEXColor(sysColor("Shade6", Session("Connection"), Session("LEX")))
            Case 9
                loadsyscolors = HEXColor(sysColor("Shade7", Session("Connection"), Session("LEX")))
            Case Else
                loadsyscolors = "#FFFFFF"
        End Select
    End Function

    Private Function formatLabels(ByVal field As Object, ByVal coltype As String)

        If coltype = "AREA" Then
            field = Format(Convert.ToDouble(field) * UPSF, "#,##0.0") & " " & UofA
        ElseIf coltype = "RENT" Then
            field = Format(field / UPSF, "c")
        ElseIf coltype = "DATE" Then
            field = CvDate(field)
        ElseIf coltype = "MONEY" Then
            field = Format(field, "c")
        ElseIf coltype = "RATIO" Then
            field = Format(field, "0.0000")
        Else

        End If
        Return field

    End Function

    Private Sub BuildDrawFlex1(ByVal SYSCOL As String)
        Dim R, Q As Integer
        Dim FT(), FR As DataRow
        Dim IT(), IR As DataRow
        Dim ColType As String
        Dim UFilter As String
        Dim UnitFilter As String = ""
        If Len(Trim(MyAction)) > 0 Then
            Dim n As Integer = ActionArray.Length - 1
            actionmode = ActionArray(n).Mode
            actiontype = ActionArray(n).Type

            'TSQL = "SELECT VName FROM " & LEX & "Themes WHERE VNo=" & ActionArray(n).View
            'ViewName = Trim(GetValue(Connection, TSQL) & "")

            Dim actionfilter As String = ActionArray(n).Filter & ";;"
            Dim F() As String = actionfilter.Split(";")
            UnitFilter = F(0) & ""
            TypeFilter = F(1) & ""
            'SaleFilter = F(2) & ""
        End If
        ''''''''''Get labels for Stack and Drawing '''''''''''''''''''''''''''''''''''''''''''''''''
        For i As Integer = 0 To 3
            OQN(i) = 0
            VQN(i) = 0
        Next
        'TSQL = "SELECT  DrawLabel FROM Users WHERE UserID='" & UserID & "'"
        'Dim Drawlabel As String = Trim(GetValue(Session("Connection"), TSQL))
        'If Len(Trim(Drawlabel)) = 0 Then
        TSQL = "SELECT  value FROM Settings WHERE Name='DrawLabel'"
        DrawLabel = GetValue(Session("Connection"), TSQL) & ""
        'End If
        If Len(Drawlabel) Then
            Dim args() = Drawlabel.Split(",")
            For i As Integer = 0 To UBound(args)
                If i > 7 Then
                ElseIf i > 3 Then
                    VQN(i - 4) = args(i)
                Else
                    OQN(i) = args(i)
                End If
            Next
        End If
        If Customary And 4 Then
            For i As Integer = 0 To 3
                If OQN(i) > 0 Then
                    oSuffix(i) = GetValue(Session("Connection"), "SELECT Suffix FROM " & Session("LEX") & "SysData WHERE QNo=" & OQN(i)) & ""
                End If
                If VQN(i) > 0 Then
                    vSuffix(i) = GetValue(Session("Connection"), "SELECT Suffix FROM " & Session("LEX") & "SysData WHERE QNo=" & VQN(i)) & ""
                End If
            Next
        End If
        Session("TimeFrame") = Format(Today(), "yyyyMMdd")
        Dim UU As DataTable = Units(Session("Connection"), Propertynumber, Session("TimeFrame"), Session("LEX"))
        UFilter = "FNo='" & FNo & "'"
        If Len(UnitFilter) Then UFilter = UFilter & " AND " & UnitFilter
        FT = UU.Select(UFilter, "UNo")
        Dim SessionQNo As String = ""
        If Len(Trim(Session("QNo") & "")) > 0 Then SessionQNo = "," & Session("QNo")
        If Len(Trim(Val(Session("QNo1")) & "")) > 0 Then SessionQNo &= "," & Val(Session("QNo1"))
        If Len(Trim(Session("QNo2") & "")) > 0 Then SessionQNo &= "," & Session("QNo2")
        If Len(Trim(Drawlabel)) > 0 Then
            TSQL = "SELECT QNo, Name, Field, DataType, Object,ColAlign FROM " & Session("LEX") & "SysData WHERE QNO IN (3,4,8,10,11,12,13,14,15,17" & SessionQNo & "," & Drawlabel & ") AND DrawCol>0"
        Else
            TSQL = "SELECT QNo, Name, Field, DataType, Object,colAlign FROM " & Session("LEX") & "SysData WHERE QNO IN (3,4,8,10,11,12,13,14,15,17" & SessionQNo & ") AND DrawCol>0"
        End If
        SysData = GetTable(Session("Connection"), TSQL)
        'Units
        IT = SysData.Select("Object='U'")

        For Each dr As DataRow In SysData.Rows
            SC(Q) = dr("QNo")
            Q += 1
        Next
        detailsList = New List(Of detailsLists)
        R = 0
        Dim ColTypeLabel As String = ""
        For Each FR In FT
            detailsList.Add(New detailsLists(0, "", "", 0, 0, 0, 0, "", "", "", "", "", "", "", "", "", "", "", "", ""))
            For Each IR In IT
                If Not IsDBNull(FR(IR("Field"))) Then
                    If Len(FR(IR("Field"))) > 0 Then
                        If IR("QNo") = 3 Or IR("QNo") = 4 Then
                            If Len(FR(IR("Field"))) > 0 Then
                                If IR("QNo") = 3 Then detailsList(R).FNo = FR(IR("Field"))
                                If IR("QNo") = 4 Then detailsList(R).UNo = FR(IR("Field"))
                                If Len(Trim(Session("QNo") & "")) > 0 Then
                                    If (Val(Session("QNo")) = 3) Or (Val(Session("QNo")) = 4) Then detailsList(R).VQNo = FR(IR("Field"))
                                End If

                                If IR("QNo") = OQN(0) Then detailsList(R).OQN0 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                                If IR("QNo") = OQN(1) Then detailsList(R).OQN1 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                                If IR("QNo") = OQN(2) Then detailsList(R).OQN2 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                                If IR("QNo") = OQN(3) Then detailsList(R).OQN3 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                                If IR("QNo") = VQN(0) Then detailsList(R).VQN0 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                                If IR("QNo") = VQN(1) Then detailsList(R).VQN1 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                                If IR("QNo") = VQN(2) Then detailsList(R).VQN2 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                                If IR("QNo") = VQN(3) Then detailsList(R).VQN3 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                            End If
                        End If
                        If IR("QNo") = 8 Or IR("QNo") = 10 Or IR("QNo") = 11 Or IR("QNo") = 12 Then
                            If Not IsDBNull(FR(IR("Field"))) Then
                                If IR("QNo") = 8 Then detailsList(R).Rentable = FR(IR("Field")) * UPSF
                                If IR("QNo") = 10 Then detailsList(R).NSF = FR(IR("Field")) * UPSF
                                If IR("QNo") = 11 Then detailsList(R).PRA = FR(IR("Field")) * UPSF
                                If IR("QNo") = 12 Then detailsList(R).CRA = FR(IR("Field")) * UPSF
                                If Len(Trim(Session("QNo") & "")) > 0 Then
                                    If (Val(Session("QNo")) = 8) Or (Session("QNo") = 10) Or (Val(Session("QNo")) = 11) Or (Val(Session("QNo")) = 12) Then detailsList(R).VQNo = FR(IR("Field")) * UPSF
                                End If

                                If IR("QNo") = OQN(0) Then detailsList(R).OQN0 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                                If IR("QNo") = OQN(1) Then detailsList(R).OQN1 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                                If IR("QNo") = OQN(2) Then detailsList(R).OQN2 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                                If IR("QNo") = OQN(3) Then detailsList(R).OQN3 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                                If IR("QNo") = VQN(0) Then detailsList(R).VQN0 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                                If IR("QNo") = VQN(1) Then detailsList(R).VQN1 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                                If IR("QNo") = VQN(2) Then detailsList(R).VQN2 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                                If IR("QNo") = VQN(3) Then detailsList(R).VQN3 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                            End If
                        End If

                        If IR("QNo") = Val(Session("QNo")) Or IR("QNo") = Val(Session("QNo1")) Or IR("QNo") = Val(Session("QNo2")) Then
                            ColType = UCase(IR("Datatype") & "")
                            If ColType = "AREA" Then
                                If Not IsDBNull(FR(IR("Field"))) Then
                                    If Val(Session("QNo")) = IR("QNo") Then detailsList(R).VQNo = Convert.ToDouble(FR(IR("Field"))) * UPSF
                                    If Val(Session("QNo1")) = IR("QNo") Then detailsList(R).Q1 = Convert.ToDouble(FR(IR("Field"))) * UPSF
                                    If Val(Session("QNo2")) = IR("QNo") Then detailsList(R).Q2 = Convert.ToDouble(FR(IR("Field"))) * UPSF
                                End If

                            ElseIf ColType = "RENT" Then
                                If Not IsDBNull(FR(IR("Field"))) Then
                                    If Val(Session("QNo")) = IR("QNo") Then detailsList(R).VQNo = FR(IR("Field")) / UPSF
                                    If Val(Session("QNo1")) = IR("QNo") Then detailsList(R).Q1 = FR(IR("Field")) / UPSF
                                    If Val(Session("QNo2")) = IR("QNo") Then detailsList(R).Q2 = FR(IR("Field")) / UPSF
                                Else
                                    If Val(Session("QNo")) = IR("QNo") Then detailsList(R).VQNo = 0
                                    If Val(Session("QNo1")) = IR("QNo") Then detailsList(R).Q1 = 0
                                    If Val(Session("QNo2")) = IR("QNo") Then detailsList(R).Q2 = 0
                                End If
                            ElseIf ColType = "DATE" Then
                                If Not IsDBNull(FR(IR("Field"))) Then
                                    If Val(Session("QNo")) = IR("QNo") Then detailsList(R).VQNo = CvDate(FR(IR("Field")))
                                    If Val(Session("QNo1")) = IR("QNo") Then detailsList(R).Q1 = CvDate(FR(IR("Field")))
                                    If Val(Session("QNo2")) = IR("QNo") Then detailsList(R).Q2 = CvDate(FR(IR("Field")))
                                End If

                            ElseIf ColType = "MONEY" Then
                                If Not IsDBNull(FR(IR("Field"))) Then
                                    If Val(Session("QNo")) = IR("QNo") Then detailsList(R).VQNo = FR(IR("Field"))
                                    If Val(Session("QNo1")) = IR("QNo") Then detailsList(R).Q1 = FR(IR("Field"))
                                    If Val(Session("QNo2")) = IR("QNo") Then detailsList(R).Q2 = FR(IR("Field"))
                                Else
                                    If Val(Session("QNo")) = IR("QNo") Then detailsList(R).VQNo = 0
                                    If Val(Session("QNo1")) = IR("QNo") Then detailsList(R).Q1 = 0
                                    If Val(Session("QNo2")) = IR("QNo") Then detailsList(R).Q2 = 0
                                End If
                            Else
                                If Val(Session("QNo")) = IR("QNo") Then detailsList(R).VQNo = Trim(FR(IR("Field")) & "")
                                If Val(Session("QNo1")) = IR("QNo") Then detailsList(R).Q1 = Trim(FR(IR("Field")) & "")
                                If Val(Session("QNo2")) = IR("QNo") Then detailsList(R).Q2 = Trim(FR(IR("Field")) & "")
                            End If
                        End If
                        If IR("QNo") = OQN(0) Then detailsList(R).OQN0 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                        If IR("QNo") = OQN(1) Then detailsList(R).OQN1 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                        If IR("QNo") = OQN(2) Then detailsList(R).OQN2 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                        If IR("QNo") = OQN(3) Then detailsList(R).OQN3 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                        If IR("QNo") = VQN(0) Then detailsList(R).VQN0 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                        If IR("QNo") = VQN(1) Then detailsList(R).VQN1 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                        If IR("QNo") = VQN(2) Then detailsList(R).VQN2 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                        If IR("QNo") = VQN(3) Then detailsList(R).VQN3 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                    End If

                    If IR("QNo") = OQN(0) Or IR("QNo") = OQN(1) Or IR("QNo") = OQN(2) Or IR("QNo") = OQN(3) Or IR("QNo") = VQN(0) Or IR("QNo") = VQN(1) Or IR("QNo") = VQN(2) Or IR("QNo") = VQN(3) Then
                        'OQN and VQN
                        ColType = UCase(IR("Datatype") & "")
                        If IR("QNo") = OQN(0) Then detailsList(R).OQN0 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                        If IR("QNo") = OQN(1) Then detailsList(R).OQN1 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                        If IR("QNo") = OQN(2) Then detailsList(R).OQN2 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                        If IR("QNo") = OQN(3) Then detailsList(R).OQN3 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                        If IR("QNo") = VQN(0) Then detailsList(R).VQN0 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                        If IR("QNo") = VQN(1) Then detailsList(R).VQN1 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                        If IR("QNo") = VQN(2) Then detailsList(R).VQN2 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                        If IR("QNo") = VQN(3) Then detailsList(R).VQN3 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))

                    End If
                End If
            Next
            R = R + 1
        Next

        If UBound(rg) Then
            For C = 1 To UBound(rg)
                Q = rg(C).ARec
                Dim CAT As Integer = fg(Q).P1 And 7
                If CAT > 3 Then
                    For R = 0 To detailsList.Count - 1
                        If UCase(detailsList(R).UNo) = UCase(Trim(fg(Q).ID)) Then
                            detailsList(R).id = Q
                            Dim UR() As DataRow = UU.Select("UNo='" & Trim(fg(Q).ID) & "'")
                            If UR.Length Then
                                Dim LabelType = UR(0)("LabelType")
                                If IsDBNull(LabelType) Or (LabelType > 3) Then LabelType = 3
                                fg(Q).P2 = UR(0)("LabelType") * 256
                            Else
                                fg(Q).P2 = 768
                            End If
                            Exit For
                        End If
                    Next
                    If R >= detailsList.Count - 1 Then 'in drawing but not in units table
                        R = detailsList.Count - 1
                        detailsList.Add(New detailsLists(0, "", "", 0, 0, 0, 0, "", "", "", "", "", "", "", "", "", "", "", "", ""))
                        If Len(UnitFilter) = 0 Or UnitFilter = "ALL" Then
                            detailsList(R).UNo = UCase(Trim(fg(Q).ID))
                            detailsList(R).id = Q
                            fg(Q).P2 = 768
                        Else
                            GoTo B005
                        End If
                    End If
                    Dim A As Double = rg(C).AArea * UPSF
                    If SC(4) Then detailsList(R).NSF = A
                    Dim PRA As Double = A * FloorRentalFactor
                    Dim CRA As Double = A * CalculatedFactor
                    Select Case BOMA
                        Case 0, 2
                            If CAT = 4 Then
                                PRA = A
                                CRA = A
                            End If
                        Case 3, 4
                            If CAT = 4 Then
                                PRA = 0
                                CRA = 0
                            End If
                    End Select
                    If SC(5) Then detailsList(R).PRA = PRA
                    If SC(6) Then detailsList(R).CRA = CRA
                    'If SC(13) Then
                    '    If SC(8) Then
                    '        .Item(R, SC(13)) = .Item(R, SC(8)) / A
                    '    End If
                    'End If
                    'If SC(14) Then
                    '    If SC(8) Then
                    '        .Item(R, SC(14)) = .Item(R, SC(8)) / (A * FloorRentalFactor)
                    '    End If
                    'End If
                End If
B005:       Next
        End If

        For Q = 1 To UBound(fg)
            If (fg(Q).C1 And 15) = 10 Then
                For R = 0 To detailsList.Count - 1
                    If UCase(detailsList(R).UNo) = UCase(Trim(fg(Q).ID)) Then Exit For
                Next
                If R = detailsList.Count - 1 Then 'Kiosk not in Units
                    detailsList.Add(New detailsLists(0, "", "", 0, 0, 0, 0, "", "", "", "", "", "", "", "", "", "", "", "", ""))
                    Dim UNo As String = UCase(Trim(fg(Q).ID))
                    If SC(1) Then detailsList(R).FNo = FNo
                    If SC(2) Then detailsList(R).UNo = UNo
                    detailsList(R).id = Q
                End If
                ' detailsList(R).id = Q  '< link details to drawing
            End If
        Next

        'Tenants
        Dim VacDef As String = "M"
        Dim TT As DataTable = Tenants(Session("Connection"), Propertynumber, Session("TimeFrame"), Session("LEX"))
        If Len(TypeFilter) Then
            FT = TT.Select(TypeFilter & " AND FNo='" & FNo & "'", "UNo, " & VacDef & "1 DESC")
        Else
            FT = TT.Select("FNo='" & FNo & "'", "UNo, " & VacDef & "1 DESC")
        End If
        IT = SysData.Select("Object='L'")
        For Each FR In TT.Rows
            For R = 0 To detailsList.Count - 1
                If UCase(detailsList(R).UNo) = UCase(FR("UNo")) Then Exit For
            Next
            If R = detailsList.Count Then GoTo nextitem
            For Each IR In IT
                If Not IsDBNull(FR(IR("Field"))) Then
                    If Len(FR(IR("Field"))) > 0 Then
                        If IR("QNo") = 15 Then
                            If Not IsDBNull(FR(IR("Field"))) Then detailsList(R).LNo = FR(IR("Field"))
                            If Len(Trim(Session("QNo") & "")) > 0 Then
                                If Val(Session("QNo")) = 15 Then detailsList(R).VQNo = FR(IR("Field"))
                            End If
                            If IR("QNo") = OQN(0) Then detailsList(R).OQN0 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                            If IR("QNo") = OQN(1) Then detailsList(R).OQN1 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                            If IR("QNo") = OQN(2) Then detailsList(R).OQN2 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                            If IR("QNo") = OQN(3) Then detailsList(R).OQN3 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                            If IR("QNo") = VQN(0) Then detailsList(R).VQN0 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                            If IR("QNo") = VQN(1) Then detailsList(R).VQN1 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                            If IR("QNo") = VQN(2) Then detailsList(R).VQN2 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                            If IR("QNo") = VQN(3) Then detailsList(R).VQN3 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                        End If
                        If IR("QNo") = 17 Then
                            If Not IsDBNull(FR(IR("Field"))) Then detailsList(R).TName = FR(IR("Field"))
                            If Len(Trim(Session("QNo") & "")) > 0 Then
                                If Val(Session("QNo")) = 17 Then detailsList(R).VQNo = FR(IR("Field"))
                            End If
                            If IR("QNo") = OQN(0) Then detailsList(R).OQN0 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                            If IR("QNo") = OQN(1) Then detailsList(R).OQN1 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                            If IR("QNo") = OQN(2) Then detailsList(R).OQN2 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                            If IR("QNo") = OQN(3) Then detailsList(R).OQN3 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                            If IR("QNo") = VQN(0) Then detailsList(R).VQN0 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                            If IR("QNo") = VQN(1) Then detailsList(R).VQN1 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                            If IR("QNo") = VQN(2) Then detailsList(R).VQN2 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                            If IR("QNo") = VQN(3) Then detailsList(R).VQN3 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                        End If
                        If IR("QNo") = Val(Session("QNo")) Or IR("QNo") = Val(Session("QNo1")) Or IR("QNo") = Val(Session("QNo2")) Then
                            ColType = UCase(IR("Datatype") & "")
                            If ColType = "AREA" Then
                                If Not IsDBNull(FR(IR("Field"))) Then
                                    If Val(Session("QNo")) = IR("QNo") Then
                                        If Len(Trim(detailsList(R).VQNo)) = 0 Then detailsList(R).VQNo = Convert.ToDouble(FR(IR("Field"))) * UPSF
                                    End If
                                    If Val(Session("QNo1")) = IR("QNo") Then detailsList(R).Q1 = Convert.ToDouble(FR(IR("Field"))) * UPSF
                                    If Val(Session("QNo2")) = IR("QNo") Then detailsList(R).Q2 = Convert.ToDouble(FR(IR("Field"))) * UPSF
                                End If

                            ElseIf ColType = "RENT" Then
                                If Not IsDBNull(FR(IR("Field"))) Then
                                    If Len(Trim(detailsList(R).VQNo)) = 0 Then
                                        If Val(Session("QNo")) = IR("QNo") Then detailsList(R).VQNo = FR(IR("Field")) / UPSF
                                    End If

                                    If Val(Session("QNo1")) = IR("QNo") Then detailsList(R).Q1 = FR(IR("Field")) / UPSF
                                    If Val(Session("QNo2")) = IR("QNo") Then detailsList(R).Q2 = FR(IR("Field")) / UPSF
                                Else
                                    If Val(Session("QNo")) = IR("QNo") Then detailsList(R).VQNo = 0
                                    If Val(Session("QNo1")) = IR("QNo") Then detailsList(R).Q1 = 0
                                    If Val(Session("QNo2")) = IR("QNo") Then detailsList(R).Q2 = 0
                                End If
                            ElseIf ColType = "DATE" Then
                                If Not IsDBNull(FR(IR("Field"))) Then
                                    If Val(Session("QNo")) = 24 Or Val(Session("QNo")) = 25 Then
                                        If (FR(IR("Field")) < Session("TimeFrame")) And ((FR("D2") >= Session("TimeFrame")) Or IsDBNull(FR("D2"))) Then
                                            detailsList(R).VQNo = CvDate(FR(IR("Field")))
                                        ElseIf (FR(IR("Field")) > Session("TimeFrame")) Then
                                            If Len(detailsList(R).VQNo) = 0 Then detailsList(R).VQNo = CvDate(FR(IR("Field")))
                                        End If
                                    ElseIf Val(Session("QNo")) = 26 Or Val(Session("QNo")) = 27 Then
                                        If Not IsDBNull(FR("M2")) Then
                                            If (FR(IR("Field")) < Session("TimeFrame")) And (FR("M2") >= Session("TimeFrame")) Then
                                                detailsList(R).VQNo = CvDate(FR(IR("Field")))
                                            ElseIf (FR(IR("Field")) > Session("TimeFrame")) Then
                                                If Len(detailsList(R).VQNo) = 0 Then detailsList(R).VQNo = CvDate(FR(IR("Field")))
                                            End If
                                        Else
                                            If (FR(IR("Field")) < Session("TimeFrame")) And IsDBNull(FR("M2")) Then
                                                detailsList(R).VQNo = CvDate(FR(IR("Field")))
                                            ElseIf (FR(IR("Field")) >= Session("TimeFrame")) Then
                                                If Len(detailsList(R).VQNo) = 0 Then detailsList(R).VQNo = CvDate(FR(IR("Field")))
                                            End If
                                        End If
                                    Else
                                        detailsList(R).VQNo = CvDate(FR(IR("Field")))
                                    End If
                                    If Val(Session("QNo1")) = IR("QNo") Then detailsList(R).Q1 = CvDate(FR(IR("Field")))
                                    If Val(Session("QNo2")) = IR("QNo") Then detailsList(R).Q2 = CvDate(FR(IR("Field")))
                                End If

                            ElseIf ColType = "MONEY" Then
                                If Not IsDBNull(FR(IR("Field"))) Then
                                    If Val(Session("QNo")) = IR("QNo") Then detailsList(R).VQNo = FR(IR("Field"))
                                    If Val(Session("QNo1")) = IR("QNo") Then detailsList(R).Q1 = FR(IR("Field"))
                                    If Val(Session("QNo2")) = IR("QNo") Then detailsList(R).Q2 = FR(IR("Field"))
                                Else
                                    If Val(Session("QNo")) = IR("QNo") Then detailsList(R).VQNo = 0
                                    If Val(Session("QNo1")) = IR("QNo") Then detailsList(R).Q1 = 0
                                    If Val(Session("QNo2")) = IR("QNo") Then detailsList(R).Q2 = 0
                                End If
                            Else
                                If Val(Session("QNo")) = IR("QNo") Then detailsList(R).VQNo = Trim(FR(IR("Field")) & "")
                                If Val(Val(Session("QNo1"))) = IR("QNo") Then detailsList(R).Q1 = Trim(FR(IR("Field")) & "")
                                If Val(Session("QNo2")) = IR("QNo") Then detailsList(R).Q2 = Trim(FR(IR("Field")) & "")
                            End If
                            If IR("QNo") = OQN(0) Then detailsList(R).OQN0 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                            If IR("QNo") = OQN(1) Then detailsList(R).OQN1 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                            If IR("QNo") = OQN(2) Then detailsList(R).OQN2 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                            If IR("QNo") = OQN(3) Then detailsList(R).OQN3 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                            If IR("QNo") = VQN(0) Then detailsList(R).VQN0 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                            If IR("QNo") = VQN(1) Then detailsList(R).VQN1 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                            If IR("QNo") = VQN(2) Then detailsList(R).VQN2 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                            If IR("QNo") = VQN(3) Then detailsList(R).VQN3 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                        End If
                        If IR("QNo") = OQN(0) Or IR("QNo") = OQN(1) Or IR("QNo") = OQN(2) Or IR("QNo") = OQN(3) Or IR("QNo") = VQN(0) Or IR("QNo") = VQN(1) Or IR("QNo") = VQN(2) Or IR("QNo") = VQN(3) Then
                            'OQN and VQN
                            If IR("QNo") = OQN(0) Then detailsList(R).OQN0 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                            If IR("QNo") = OQN(1) Then detailsList(R).OQN1 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                            If IR("QNo") = OQN(2) Then detailsList(R).OQN2 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                            If IR("QNo") = OQN(3) Then detailsList(R).OQN3 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                            If IR("QNo") = VQN(0) Then detailsList(R).VQN0 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                            If IR("QNo") = VQN(1) Then detailsList(R).VQN1 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                            If IR("QNo") = VQN(2) Then detailsList(R).VQN2 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                            If IR("QNo") = VQN(3) Then detailsList(R).VQN3 = formatLabels(Trim(FR(IR("Field")) & ""), UCase(IR("Datatype") & ""))
                        End If
                    End If
                End If
            Next
nextitem: Next
        '*******************************
        'IT = DT.Select("Object='Q'")
        'If IT.Length Then
        '    TSQL = "SELECT * FROM SurveyResults Where (PNo='" & PropertyNumber & "')"
        '    TT = GetTable(Connection, TSQL)
        '    If TT.Rows.Count Then
        '        FT = TT.Select("Floor='" & FloorNumber & "'")
        '        For Each FR In FT
        '            For R = 1 To .Rows.Count - 1
        '                If UCase(.Item(R, SC(4))) = UCase(FR("UNo")) Then Exit For
        '            Next
        '            If R < .Rows.Count Then
        '                For Each IR In IT
        '                    C = SC(IR("QNo"))
        '                    .Item(R, C) = FR(IR("Field"))
        '                Next
        '            End If
        '        Next
        '    End If
        'End If
        '********************************************
    End Sub


    Public Shared Function svgEsc(ByVal text) As String
        Dim escape As String = text
        If InStr(text, "&") Then escape = Replace(escape, "&", "&amp;")
        If InStr(text, "'") Then escape = Replace(escape, "'", "&apos;")
        If InStr(text, Chr(34)) Then escape = Replace(escape, Chr(34), "&quot;")
        If InStr(text, "<") Then escape = Replace(escape, "<", "&lt;")
        If InStr(text, ">") Then escape = Replace(escape, ">", "&gt;")
        For n As Integer = 162 To 255
            If InStr(escape, Chr(n)) Then escape = Replace(escape, Chr(n), "&#" & CStr(n) & ";")
        Next
        Return escape
    End Function

    Private Sub LoadMap(ByVal MNo As Integer)
        ReDim fg(0)
        ReDim lg(0)
        ReDim bg(0)
        ReDim rg(0)
        ReDim zoo(0)
        Dim byteMe() As Byte
        byteMe = GetBytes(Session("Connection"), "SELECT Map FROM Maps WHERE MNo=" & CStr(MNo))
        If IsNothing(byteMe) Then Exit Sub
        Dim fms As New MemoryStream(byteMe)
        Dim fbr As New BinaryReader(fms)
        Dim i, j, n As Integer
        Dim s(IDLEN - 1) As Byte
        n = UBound(byteMe) / (24 + IDLEN)
        ReDim fg(n)
        For i = 1 To n
            fg(i).C1 = fbr.ReadInt16
            fg(i).C2 = fbr.ReadInt16
            s = fbr.ReadBytes(IDLEN)
            Dim sb As New StringBuilder(IDLEN)
            For j = 0 To IDLEN - 1
                If s(j) < 32 Then s(j) = 32
                sb.Append(Chr(s(j)))
            Next
            fg(i).ID = RTrim(sb.ToString)
            fg(i).P1 = fbr.ReadInt16
            fg(i).P2 = fbr.ReadInt16
            fg(i).X1 = fbr.ReadSingle
            fg(i).X2 = fbr.ReadSingle
            fg(i).Y1 = fbr.ReadSingle
            fg(i).Y2 = fbr.ReadSingle
        Next
        fbr.Close()
        fms.Close()
        ReDim byteMe(0)

        TSQL = "SELECT Name, DXFName, DXFScale, Marketing1, Marketing2, Calculated, Rentable, Size1, Size2, Size3, Size4, Size5, Size6, Size7, Style4 FROM Maps WHERE (MNo=" & CStr(MNo) & ")"
        Dim DT As DataTable = GetTable(Session("Connection"), TSQL)
        Dim DR As DataRow = DT.Rows(0)
        Dim DrawingName As String = DR("Name") & ""
        Dim DXFName As String = DR("DXFName") & ""
        DXFScale = DR("DXFScale")
        Dim Marketing1 As String = DR("Marketing1") & ""
        Dim Marketing2 = DR("Marketing2") & ""
        Dim FloorRentable, Calculated
        Dim DimensionFormat
        If IsDBNull(DR("Rentable")) Then
            FloorRentable = 0
        Else
            FloorRentable = DR("Rentable")
        End If
        If IsDBNull(DR("Calculated")) Then
            Calculated = DR("Rentable")
        Else
            Calculated = DR("Calculated")
        End If
        FontSize1 = DR("Size1")
        FontSize2 = DR("Size2")
        FontSize3 = DR("Size3")
        FontSize4 = DR("Size4")
        If Not IsDBNull(DR("Size5")) Then FontSize5 = DR("Size5")
        If Not IsDBNull(DR("Size6")) Then FontSize6 = DR("Size6")
        If Not IsDBNull(DR("Size7")) Then FontSize7 = DR("Size7")
        DimensionFormat = DR("Style4")
        If Len(DXFName) Then
            TSQL = "SELECT XFile FROM XData WHERE (PNo='" & CStr(Propertynumber) & "') AND (FileName='" & DXFName & "')"
            byteMe = GetBytes(Session("Connection"), TSQL)
            If Not IsNothing(byteMe) Then
                If byteMe.Length Then
                    n = byteMe.Length / 10
                    Dim bms As New MemoryStream(byteMe)
                    Dim bbr As New BinaryReader(bms)
                    ReDim bg(n)
                    For i = 1 To n
                        bg(i).W = bbr.ReadInt16
                        bg(i).X = bbr.ReadSingle
                        bg(i).Y = bbr.ReadSingle
                    Next
                    bbr.Close()
                    bms.Close()
                    ReDim byteMe(0)
                End If
            End If
            TSQL = "SELECT XNo FROM XData WHERE (PNo='" & CStr(Propertynumber) & "') AND (FileName='" & DXFName & "')"
            Dim XNo As Integer = GetValue(Session("Connection"), TSQL)
            If UBound(bg) = 0 Then LoadLayers(MNo)
        End If
        LoadCustomLabels(MNo)
        If Customary And 128 Then LoadLogos()
        If UBound(fg) Then SortMap()
        MinMax()
    End Sub

    Private Sub LoadLogos()
        ReDim lg(0)
        For irec As Integer = 1 To UBound(fg)
            If (fg(irec).C1 And 15) = 13 Then
                Dim Logo As String = Trim(fg(irec).ID)
                Dim i As Integer
                Dim byteMe() As Byte
                byteMe = GetBytes(Session("Connection"), "SELECT iData FROM LogoLibrary WHERE iName='" & Apostrify(Logo) & "'")
                If Not IsNothing(byteMe) Then
                    Dim n As Integer = UBound(byteMe) / 10
                    Dim ms As New MemoryStream(byteMe)
                    Dim br As New BinaryReader(ms)
                    Dim ig(n) As PXF
                    For i = 1 To n
                        ig(i).W = br.ReadInt16
                        ig(i).X = br.ReadSingle
                        ig(i).Y = br.ReadSingle
                    Next
                    br.Close()
                    ms.Close()
                    If fg(irec).X2 Then RotateLogo(ig, fg(irec).X2)
                    If fg(irec).Y2 <> 1 Then ScaleLogo(ig, fg(irec).Y2)
                    TranslateLogo(ig, fg(irec).X1, fg(irec).Y1)
                    Dim j As Integer = UBound(lg)
                    ReDim Preserve lg(j + n)
                    For m As Integer = 1 To n
                        Dim k As Integer = j + m
                        lg(k).W = ig(m).W
                        lg(k).X = ig(m).X
                        lg(k).Y = ig(m).Y
                    Next
                    ReDim byteMe(0)
                End If
            End If
        Next
    End Sub

    Private Sub TranslateLogo(ByRef ig() As PXF, ByVal X As Integer, ByVal Y As Integer)
        For i As Integer = 1 To UBound(ig)
            If ig(i).W <> -32768 Then
                Select Case Math.Abs(ig(i).W)
                    Case 0 To 21599, 32000, 32767
                        ig(i).X = ig(i).X + X
                        ig(i).Y = ig(i).Y + Y
                    Case Else

                End Select
            End If
        Next
    End Sub

    Private Sub RotateLogo(ByRef ig() As PXF, ByVal angle As Single)
        Dim x, y As Single
        Dim theta As Double = Math.Atan(1) / 45
        Dim SI As Double = Math.Sin(theta * angle)
        Dim CO As Double = Math.Cos(theta * angle)
        For i As Integer = 1 To UBound(ig)
            If ig(i).W <> -32768 Then
                Select Case Math.Abs(ig(i).W)
                    Case 0 To 21599, 32000, 32767
                        x = ig(i).X * CO - ig(i).Y * SI
                        y = ig(i).X * SI + ig(i).Y * CO
                        ig(i).X = x
                        ig(i).Y = y
                    Case Else

                End Select
            End If
        Next
    End Sub

    Private Sub ScaleLogo(ByRef ig() As PXF, ByVal Scale As Single)
        For i As Integer = 1 To UBound(ig)
            If ig(i).W <> -32768 Then
                Select Case Math.Abs(ig(i).W)
                    Case 0 To 21600, 32000, 32767
                        ig(i).X = ig(i).X * Scale
                        ig(i).Y = ig(i).Y * Scale

                    Case Else

                End Select
            End If
        Next
    End Sub

    Private Sub MinMax()
        Dim i As Integer
        Dim dx, dy As Single
        Dim pass As Boolean

        fx1 = 0
        fy1 = 0
        fx2 = 0
        fy2 = 0

        If UBound(bg) > 2 Then
            For i = 1 To UBound(bg)
                If bg(i).W = -32768 Then Exit For
                Select Case Math.Abs(bg(i).W)
                    Case 0 To 21599, 32767
                        fx1 = bg(i).X
                        fx2 = bg(i).X
                        fy1 = bg(i).Y
                        fy2 = bg(i).Y
                        Exit For
                End Select
            Next
        Else
            If UBound(fg) > 1 Then
                For i = 1 To UBound(fg)
                    If (fg(i).C1 > 0) And (fg(i).C1 < 192) Then
                        Select Case (fg(i).C1)
                            Case 1, 2, 3
                                fx1 = fg(i).X1
                                fy1 = fg(i).Y1
                                fx2 = fg(i).X1
                                fy2 = fg(i).Y1
                                Exit For
                        End Select
                    End If
                Next
            End If
        End If

        If UBound(lg) > 0 Then
            For i = 1 To UBound(lg)
                If lg(i).W <> -32768 Then
                    Select Case Math.Abs(lg(i).W)
                        Case 0 To 21599, 32767
                            If lg(i).X < fx1 Then fx1 = lg(i).X
                            If lg(i).X > fx2 Then fx2 = lg(i).X
                            If lg(i).Y < fy1 Then fy1 = lg(i).Y
                            If lg(i).Y > fy2 Then fy2 = lg(i).Y
                    End Select
                End If
            Next
        End If

        If UBound(bg) > 0 Then
            For i = 1 To UBound(bg)
                If bg(i).W <> -32768 Then
                    Select Case Math.Abs(bg(i).W)
                        Case 0 To 21599, 32767
                            If bg(i).X < fx1 Then fx1 = bg(i).X
                            If bg(i).X > fx2 Then fx2 = bg(i).X
                            If bg(i).Y < fy1 Then fy1 = bg(i).Y
                            If bg(i).Y > fy2 Then fy2 = bg(i).Y
                    End Select
                End If
            Next
        End If

        If UBound(fg) > 0 Then
            For i = 1 To UBound(fg)
                If (fg(i).C1 > 0) And (fg(i).C1 < 192) Then
                    Select Case (fg(i).C1 And 15)
                        Case 1, 2, 3, 4, 12, 14
                            If fg(i).X1 < fx1 Then fx1 = fg(i).X1
                            If fg(i).X1 > fx2 Then fx2 = fg(i).X1
                            If fg(i).Y1 < fy1 Then fy1 = fg(i).Y1
                            If fg(i).Y1 > fy2 Then fy2 = fg(i).Y1
                            If fg(i).X2 < fx1 Then fx1 = fg(i).X2
                            If fg(i).X2 > fx2 Then fx2 = fg(i).X2
                            If fg(i).Y2 < fy1 Then fy1 = fg(i).Y2
                            If fg(i).Y2 > fy2 Then fy2 = fg(i).Y2
                        Case 5, 7
                            If fg(i).X1 < fx1 Then fx1 = fg(i).X1
                            If fg(i).X1 > fx2 Then fx2 = fg(i).X1
                            If fg(i).Y1 < fy1 Then fy1 = fg(i).Y1
                            If fg(i).Y1 > fy2 Then fy2 = fg(i).Y1

                            If fg(i).X2 > fx2 Then fx2 = fg(i).X2

                        Case 6, 10, 13
                            If fg(i).X1 < fx1 Then fx1 = fg(i).X1
                            If fg(i).X1 > fx2 Then fx2 = fg(i).X1
                            If fg(i).Y1 < fy1 Then fy1 = fg(i).Y1
                            If fg(i).Y1 > fy2 Then fy2 = fg(i).Y1

                        Case 8
                            If fg(i).X1 < fx1 Then fx1 = fg(i).X1
                            If fg(i).X1 > fx2 Then fx2 = fg(i).X1
                            If fg(i).Y1 < fy1 Then fy1 = fg(i).Y1
                            If fg(i).Y1 > fy2 Then fy2 = fg(i).Y1
                            pass = False
                            If fg(i).P2 > 0 Then
                                pass = True
                            Else
                                If (fg(fg(i).P1).C2 And 1) = 0 Then pass = True
                            End If
                            If pass Then
                                If fg(i).X2 < fx1 Then fx1 = fg(i).X2
                                If fg(i).X2 > fx2 Then fx2 = fg(i).X2
                                If fg(i).Y2 < fy1 Then fy1 = fg(i).Y2
                                If fg(i).Y2 > fy2 Then fy2 = fg(i).Y2
                            End If

                        Case 9
                            Dim z As Single
                            Select Case fg(i).P2 \ 256
                                Case 1
                                    z = 8 * FontSize1 / DXFScale
                                Case 2
                                    z = 8 * FontSize2 / DXFScale
                                Case Else
                                    z = 8 * FontSize3 / DXFScale
                            End Select
                            Dim x1 As Single = fg(i).X2 - z
                            Dim x2 As Single = fg(i).X2 + z
                            Dim y1 As Single = fg(i).Y2 - z
                            Dim y2 As Single = fg(i).Y2 + z
                            If x1 < fx1 Then fx1 = x1
                            If x1 > fx2 Then fx2 = x1
                            If y1 < fy1 Then fy1 = y1
                            If y1 > fy2 Then fy2 = y1
                            If x2 < fx1 Then fx1 = x2
                            If x2 > fx2 Then fx2 = x2
                            If y2 < fy1 Then fy1 = y2
                            If y2 > fy2 Then fy2 = y2

                    End Select
                End If
            Next i
        End If

        If (fx1 = fx2) And (fy1 = fy2) Then 'deleted everything
            fx1 = 0
            fy1 = 0
            fx2 = 1200
            fy2 = 1200
        End If

        dx = (fx2 - fx1) / 50
        dy = (fy2 - fy1) / 50
        fx1 = fx1 - dx
        fy1 = fy1 - dy
        fx2 = fx2 + dx
        fy2 = fy2 + dy

    End Sub

    Private Sub SortMap()
        Dim i, j, k, m As Integer
        Dim n, side() As Integer
        Dim a, area() As Single
        ReDim area(0)
        ReDim side(0)
        m = 0 'base used for sorting
        ReDim rg(0)
        For i = 1 To UBound(fg)
            If fg(i).C1 = 9 Then
                m = m + 1
                ReDim Preserve rg(m)
                rg(m).ARec = i
                rg(m).BRec = 0
                rg(m).AArea = 0
                rg(m).BArea = 0
                rg(m).Parent = 0
            End If
        Next
        If m = 0 Then Exit Sub
        n = 0
        For i = 1 To UBound(fg)
            If fg(i).C1 < 192 Then
                If (fg(i).C1 And 15) = 1 Then
                    If (fg(i).C1 And 32) = 0 Then
                        a = sloop(1, i) 'tag
                        If a > 0 Then
                            n = n + 1
                            ReDim Preserve side(n)
                            ReDim Preserve area(n)
                            side(n) = i
                            area(n) = a
                        End If
                    End If
                    If (fg(i).C1 And 16) = 0 Then
                        a = sloop(1, -i) 'tag
                        If a > 0 Then
                            n = n + 1
                            ReDim Preserve side(n)
                            ReDim Preserve area(n)
                            side(n) = -i
                            area(n) = a
                        End If
                    End If
                End If
            End If
        Next
        For i = 1 To UBound(fg)
            fg(i).C1 = fg(i).C1 And 207
        Next i
        For i = 1 To n - 1
            For j = i + 1 To n
                If area(i) > area(j) Then 'sort spaces small to big
                    area(0) = area(j)
                    area(j) = area(i)
                    area(i) = area(0)
                    side(0) = side(j)
                    side(j) = side(i)
                    side(i) = side(0)
                End If
            Next
        Next
        For i = 1 To n
            a = sloop(0, side(i))
            For j = 1 To m
                If rg(j).BRec = 0 Then
                    If inside(fg(rg(j).ARec).X1, fg(rg(j).ARec).Y1) Then
                        rg(j).AArea = a
                        rg(j).BArea = a
                        For k = 1 To m
                            If rg(k).BRec <> 0 Then
                                If inside(fg(rg(k).ARec).X1, fg(rg(k).ARec).Y1) Then
                                    rg(j).AArea = rg(j).AArea - rg(k).AArea
                                    If rg(k).Parent = 0 Then rg(k).Parent = rg(j).ARec
                                End If
                            End If
                        Next
                        rg(j).BRec = side(i)
                        Exit For
                    End If
                End If
            Next
        Next
        ''''''''''''''''''''''''''''''''''''''''''
        For i = 1 To 7
            NetArea(i) = 0
        Next
        For i = 1 To m
            n = fg(rg(i).ARec).P1 And 7
            NetArea(n) = NetArea(n) + rg(i).AArea
        Next
        FloorRentalFactor = 1
        If FloorRentable Then
            Select Case BOMA
                Case 1
                    If NetArea(4) + NetArea(5) Then
                        FloorRentalFactor = FloorRentable / (NetArea(4) + NetArea(5))
                    End If
                Case 0, 2
                    If NetArea(5) Then
                        FloorRentalFactor = (FloorRentable - NetArea(4)) / NetArea(5)
                    End If
                Case Else
                    If NetArea(5) Then
                        FloorRentalFactor = FloorRentable / NetArea(5)
                    End If
            End Select
        End If
        CalculatedFactor = 1
        If Calculated Then
            Select Case BOMA
                Case 1
                    If NetArea(4) + NetArea(5) Then
                        CalculatedFactor = Calculated / (NetArea(4) + NetArea(5))
                    End If
                Case 0, 2
                    If NetArea(5) Then
                        CalculatedFactor = (Calculated - NetArea(4)) / NetArea(5)
                    End If
                Case Else
                    If NetArea(5) Then
                        CalculatedFactor = Calculated / NetArea(5)
                    End If
            End Select
        End If
        '''''''''''''''''''''''''''''''''''''''''
        For i = 1 To m - 1
            For j = i + 1 To m
                If rg(i).BArea < rg(j).BArea Then 'sort descending
                    rg(0) = rg(j)
                    rg(j) = rg(i)
                    rg(i) = rg(0)
                End If
            Next
        Next
    End Sub

    Function inside(ByVal X As Single, ByVal Y As Single) As Boolean
        Dim Inflag, i As Short
        Dim x1, Y1 As Single
        Dim x2, Y2 As Single
        Dim dx1, dy1 As Single
        Dim dx2, dy2 As Single
        Dim B, a, C As Single
        inside = False
        If UBound(circuit) < 4 Then Exit Function
        If X < lx1 Then Exit Function
        If Y < ly1 Then Exit Function
        If X > lx2 Then Exit Function
        If Y > ly2 Then Exit Function

        Inflag = 0
        For i = 2 To UBound(circuit)
            If circuit(i).W Then
                x1 = circuit(i - 1).X
                Y1 = circuit(i - 1).Y
                x2 = circuit(i).X
                Y2 = circuit(i).Y
                Call lineq(x1, Y1, x2, Y2, a, B, C)
                If (a * X + B * Y + C) = 0 Then
                    If onseg(X, Y, x1, Y1, x2, Y2) > 0 Then Exit Function
                End If
                dx1 = x1 - X
                dx2 = x2 - X
                If (dx1 * dx2) < 0 Then
                    dy1 = Y1 - Y
                    dy2 = Y2 - Y
                    If (dy1 / Math.Abs(dx1)) + (dy2 / Math.Abs(dx2)) > 0 Then Inflag = 1 - Inflag
                End If
            End If
        Next
        If Inflag Then inside = True
    End Function

    Private Function sloop(ByVal Tag As Integer, ByVal snaprec As Integer) As Single

        Dim n, irec As Integer
        Dim lastrec, nextrec As Integer
        Dim X, Y As Single
        Dim twice As Single

        sloop = 0
        If snaprec = 0 Then Exit Function
        n = 0
        lastrec = snaprec
        nextrec = 0
        Do Until nextrec = snaprec
            irec = Math.Abs(lastrec)
            If lastrec < 0 Then
                If Tag = 1 Then fg(irec).C1 = fg(irec).C1 Or 16
                nextrec = fg(irec).P2
                X = fg(irec).X2
                Y = fg(irec).Y2
            Else
                If Tag = 1 Then fg(irec).C1 = fg(irec).C1 Or 32
                nextrec = fg(irec).P1
                X = fg(irec).X1
                Y = fg(irec).Y1
            End If
            If nextrec = 0 Then
                Return 0
                Exit Function
            End If
sloop10:    If (fg(Math.Abs(nextrec)).C1 And 192) = 192 Then
                If nextrec < 0 Then
                    nextrec = fg(Math.Abs(nextrec)).P1
                Else
                    nextrec = fg(nextrec).P2
                End If
                GoTo sloop10
            End If
            n = n + 1
            If n = 5000 Then
                MsgBox(Resource("Drawing", "Msg20", "Endless loop at record") & " (" & CStr(snaprec) & ")")
                Return 0
            End If
            ReDim Preserve circuit(n)
            circuit(n).X = X
            circuit(n).Y = Y
            If n = 1 Then
                lx1 = X
                ly1 = Y
                lx2 = X
                ly2 = Y
                twice = 0
                circuit(n).W = 0

            Else
                circuit(n).W = 1
                If X < lx1 Then lx1 = X
                If Y < ly1 Then ly1 = Y
                If X > lx2 Then lx2 = X
                If Y > ly2 Then ly2 = Y
                twice = twice - (circuit(n - 1).X + circuit(n).X) * (circuit(n - 1).Y - circuit(n).Y)
            End If
            lastrec = nextrec
        Loop
        n = n + 1
        ReDim Preserve circuit(n)
        circuit(n).W = 1
        circuit(n).X = circuit(1).X
        circuit(n).Y = circuit(1).Y
        twice = (twice - (circuit(n - 1).X + circuit(n).X) * (circuit(n - 1).Y - circuit(n).Y))
        If METRIC Then
            sloop = twice / (2 * DXFScale * DXFScale)
        Else
            sloop = twice / (2 * (12 / DXFScale) ^ 2)
        End If
    End Function

    Private Sub LoadCustomLabels(ByVal MNo As Integer)
        ReDim Custom(0)
        TSQL = "SELECT * FROM Labels WHERE MNo=" & MNo
        Dim DT As DataTable = GetTable(Session("Connection"), TSQL)
        Dim N As Integer = DT.Rows.Count
        If N Then
            ReDim Custom(N)
            N = 0
            For Each DR As DataRow In DT.Rows
                N = N + 1
                Custom(N).RNo = DR("RNo")
                Custom(N).Label = DR("Label") & ""
                Custom(N).Size = DR("Size")
                Custom(N).Angle = DR("Angle")
                Custom(N).Hatch = DR("Hatch")
                Custom(N).Color = DR("Color")
                If IsDBNull(DR("Type")) Then
                    Custom(N).Type = 0
                Else
                    Custom(N).Type = DR("Type")
                End If
            Next
        End If
        ReDim Market(0)
        TSQL = "SELECT Value FROM Settings WHERE [Name]='SalesZoneField'"
        SalesZoneField = GetValue(Session("Connection"), TSQL) & ""
        If Len(SalesZoneField) Then
            TSQL = "SELECT a.*, b.Value AS Zone FROM CATSALESFEEDDETCOMB a INNER JOIN UNitData b ON a.PNo=b.PNo AND a.UNo=b.UNo WHERE a.PNo=@PNO AND b.Name=@SZN"
            PRM = "@PNO=" & Propertynumber & ";@SZN=" & SalesZoneField
            Dim ST As DataTable = GetTableEx(Session("Connection"), TSQL, PRM)
            If ST.Rows.Count Then
                TSQL = "SELECT * FROM " & Session("Lex") & "SalesZones WHERE PNo=@PNO"
                PRM = "@PNO=" & Propertynumber
                DT = GetTableEx(Session("Connection"), TSQL, PRM)
                N = DT.Rows.Count
                If N Then
                    ReDim Market(N)
                    N = 0
                    For Each DR As DataRow In DT.Rows
                        N = N + 1
                        Market(N).ZNo = DR("ZNo")
                        Market(N).ZName = "Zone " & DR("ZNo")
                        If IsDBNull(DR("Color")) Then
                            Market(N).Color = Color.DarkGray.ToArgb
                        Else
                            Market(N).Color = DR("Color")
                        End If
                        Dim SS() As DataRow = ST.Select("Zone='" & DR("ZNo") & "'")
                        Market(N).Label = ""
                    Next
                End If
            End If
        End If
    End Sub

    Private Sub LoadLayers(ByVal MNo As Integer)
        Dim byteMe() As Byte
        TSQL = "SELECT a.XXX, b.Color, b.Visible, b.LineWidth FROM Layers AS a INNER JOIN LayerState AS b ON a.XXX=b.XXX WHERE b.MNo=@MNO"
        PRM = "@MNO=" & MNo
        Dim DT As DataTable = GetTableEx(Session("Connection"), TSQL, PRM)
        For Each DR As DataRow In DT.Rows
            If DR("Visible") Then
                If UBound(bg) = 0 Then ReDim bg(1)
                Dim j As Integer = UBound(bg)
                bg(j).W = 29000
                If IsDBNull(DR("Color")) Then
                    bg(j).X = 0
                Else
                    bg(j).X = DR("Color")
                End If
                If IsDBNull(DR("LineWidth")) Then
                    bg(j).Y = 0
                Else
                    bg(j).Y = DR("LineWidth")
                End If
                Dim XXX As Integer = DR("XXX")
                byteMe = GetBytes(Session("Connection"), "SELECT XFile FROM Layers WHERE XXX=" & CStr(XXX))
                If Not IsNothing(byteMe) Then
                    Dim n As Integer = UBound(byteMe) / 10
                    Dim ms As New MemoryStream(byteMe)
                    Dim br As New BinaryReader(ms)
                    ReDim Preserve bg(j + n)
                    For m As Integer = 1 To n
                        Dim k As Integer = j + m
                        bg(k).W = br.ReadInt16
                        bg(k).X = br.ReadSingle
                        bg(k).Y = br.ReadSingle
                    Next
                    br.Close()
                    ms.Close()
                    ReDim byteMe(0)
                End If
            End If
        Next
    End Sub

End Class

Public Class detailsLists
    Public id As Integer
    Public FNo As String
    Public UNo As String
    Public Rentable As Double
    Public NSF As Double
    Public PRA As Double
    Public CRA As Double
    Public LNo As String
    Public TName As String
    Public VQNo As Object
    Public Q1 As Object
    Public Q2 As Object
    Public OQN0 As Object
    Public OQN1 As Object
    Public OQN2 As Object
    Public OQN3 As Object
    Public VQN0 As Object
    Public VQN1 As Object
    Public VQN2 As Object
    Public VQN3 As Object


    Public Sub New(ByVal l_id As Integer, ByVal l_FNo As String, ByVal l_UNo As String, ByVal l_Rentable As Double, ByVal l_NSF As Double, ByVal l_PRA As Double, ByVal l_CRA As Double, ByVal l_LNo As String, ByVal l_TName As String, ByVal l_VQno As Object, ByVal l_Q1 As Object, ByVal l_Q2 As Object, ByVal l_OQN0 As Object, ByVal l_OQN1 As Object, ByVal l_OQN2 As Object, ByVal l_OQN3 As Object, ByVal l_VQN0 As Object, ByVal l_VQN1 As Object, ByVal l_VQN2 As Object, ByVal l_VQN3 As Object)
        id = l_id
        FNo = l_FNo
        UNo = l_UNo
        Rentable = l_Rentable
        NSF = l_NSF
        PRA = l_PRA
        CRA = l_CRA
        LNo = l_LNo
        TName = l_TName
        VQNo = l_VQno
        Q1 = l_Q1
        Q2 = l_Q2
        OQN0 = l_OQN0
        OQN1 = l_OQN1
        OQN2 = l_OQN2
        OQN3 = l_OQN3
        VQN0 = l_VQN0
        VQN1 = l_VQN1
        VQN2 = l_VQN2
        VQN3 = l_VQN3
    End Sub
End Class

Public Class viewLists
    Public VNo As Integer
    Public VType As String
    Public QNo As Integer
    Public ColorTable As String
    Public Vacant As Boolean
    Public Q1 As Integer
    Public Q2 As Integer
    Public OptionCode As String
    Public ColorField As String = ""

    Public Sub New(ByVal l_VNo As Integer, ByVal l_VType As String, ByVal l_QNo As String, ByVal l_ColorTable As String, ByVal l_Vacant As Boolean, ByVal l_Q1 As Integer, ByVal l_Q2 As Integer, ByVal l_OptionCode As String, ByVal l_ColorField As String)
        VType = l_VType
        QNo = l_QNo
        ColorTable = l_ColorTable
        VNo = l_VNo
        Vacant = l_Vacant
        Q1 = l_Q1
        Q2 = l_Q2
        OptionCode = l_OptionCode
        ColorField = l_ColorField
    End Sub
End Class

Public Class colorTop10
    Dim Uno As String
    Public color As String
    Public description As String
    Public RSF As Double
    Public Sub New(ByVal l_Uno As String, ByVal l_color As String, ByVal l_description As String, ByVal l_RSF As Double)
        Uno = l_Uno
        color = l_color
        description = l_description
        RSF = l_RSF
    End Sub
    Public Function find(ByVal colorT As colorTop10) As Boolean
        Return colorT.description = description
    End Function


End Class

