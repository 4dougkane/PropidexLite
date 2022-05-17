
Imports System.Data
Imports System.Data.SqlClient

Public Class SQLServer
    Public Shared TSQL As String = ""
    Public Shared Custom() As CustomLabel
    Public Shared PRM As String
    Public Shared SalesZoneField As String
    Public Shared LanguageResources As DataTable
    Public Shared UofM As String = "F"
    Public Shared UofA As String = "SF"
    Public Shared UPSF As Single = 1
    Public Shared PER As String = "PSF"
    Public Shared MyView As String = "PSF"
    Public Shared MyAction As String = "PSF"

    Structure ActionObject
        Dim Name As String
        Dim Mode As Integer
        Dim Type As Integer
        Dim Filter As String
        Dim View As Integer
    End Structure


    Structure MarkerLabel
        Dim ZNo As String
        Dim ZName As String
        Dim Label As String
        Dim Color As Integer
    End Structure

    Structure CustomLabel
        Dim RNo As Integer
        Dim Label As String
        Dim Size As Integer
        Dim Angle As Integer
        Dim Color As Integer
        Dim Hatch As Integer
        Dim Type As Integer
    End Structure
    Structure MapType
        Dim C1 As Short
        Dim C2 As Short
        Dim ID As String
        Dim P1 As Short
        Dim P2 As Short
        Dim X1 As Single
        Dim X2 As Single
        Dim Y1 As Single
        Dim Y2 As Single
    End Structure

    Structure PXF
        Dim W As Short
        Dim X As Single
        Dim Y As Single
    End Structure

    Structure SPIN
        Dim ARec As Integer 'room record
        Dim BRec As Integer 'boundary record
        Dim AArea As Single 'actual area
        Dim BArea As Single 'boundary area
        Dim Parent As Integer
    End Structure

    Structure ZPAIR
        Dim Record As Integer
        Dim Color As Integer
        Dim Linetype As String
    End Structure

    Public Shared Function HEXColor(ByVal argb As Integer) As String
        Return "#" & Microsoft.VisualBasic.Right("00000" & Hex(argb), 6)
    End Function

    Public Shared Function sysColor(ByVal ColorName As String, ByVal Connection As String, ByVal Lex As String) As Integer
        sysColor = 0
        If TableExists(Connection, Lex & "sysColors") Then
            TSQL = "SELECT Color FROM " & Lex & "sysColors WHERE Description='" & ColorName & "'"
            Dim sys = GetValue(Connection, TSQL) & ""
            If IsNumeric(sys) Then sysColor = sys
        End If
    End Function


    Public Shared Function Resource(ByVal Form As String, ByVal Item As String, ByVal ElseDefault As String) As String
        Dim R() As DataRow = LanguageResources.Select("Form='" & Form & "' AND Item='" & Item & "'")
        If R.Length Then
            If Len(R(0)("Caption") & "") = 0 Then
                Resource = ElseDefault
            Else
                Resource = R(0)("Caption")
            End If
        Else
            Resource = ElseDefault
        End If
    End Function


    Public Shared Function onseg(ByVal X As Single, ByVal Y As Single, ByVal X1 As Single, ByVal Y1 As Single, ByVal X2 As Single, ByVal Y2 As Single) As Short
        onseg = 1
        If (Math.Abs(X - X1) < 0.01) And (Math.Abs(Y - Y1) < 0.01) Then Exit Function
        If (Math.Abs(X - X2) < 0.01) And (Math.Abs(Y - Y2) < 0.01) Then Exit Function
        onseg = 0
        If X1 = X2 Then
            If Math.Abs(X - X1) > 0.01 Then Exit Function
        Else
            If X < X1 And X < X2 Then Exit Function
            If X > X1 And X > X2 Then Exit Function
        End If
        If Y1 = Y2 Then
            If Math.Abs(Y - Y1) > 0.01 Then Exit Function
        Else
            If Y < Y1 And Y < Y2 Then Exit Function
            If Y > Y1 And Y > Y2 Then Exit Function
        End If
        onseg = 2
    End Function


    Public Shared Function angle(ByVal x1 As Single, ByVal y1 As Single, ByVal x2 As Single, ByVal y2 As Single) As Integer
        Dim mins As Integer
        Dim dx, dy As Single
        mins = 0
        dx = x2 - x1
        dy = y2 - y1
        If dx < 0 Then
            mins = 10800
            If dy <> 0 Then mins = mins + Math.Atan(dy / dx) * (2700 / Math.Atan(1))
        ElseIf dx = 0 Then
            If dy < 0 Then
                mins = 16200
            ElseIf dy > 0 Then
                mins = 5400
            End If
        Else
            If dy <> 0 Then
                mins = Math.Atan(dy / dx) * (2700 / Math.Atan(1))
                If dy < 0 Then mins = mins + 21600
            End If
        End If
        angle = mins
    End Function

    Public Shared Sub lineq(ByVal X1 As Single, ByVal Y1 As Single, ByVal X2 As Single, ByVal Y2 As Single, ByRef A As Double, ByRef B As Double, ByRef C As Double)
        A = Y2 - Y1
        B = X1 - X2
        C = -B * Y1 - A * X1
    End Sub

    Public Shared Function TableExists(ByVal connection As String, ByVal TableName As String) As Boolean
        Dim source As String = "SELECT OBJECT_ID('" & TableName & "','U')"
        If Len(connection) Then
            Dim cn As New SqlConnection(connection)
            Try
                cn.Open()
                Dim co As New SqlCommand(source, cn)
                TableExists = Not IsDBNull(co.ExecuteScalar)
                cn.Dispose()
            Catch ex As System.Exception
                TableExists = False
                If cn.State = ConnectionState.Open Then cn.Dispose()
            End Try
        Else
            Try
                Dim prox As New PropidexLite.WebData.Service
                prox.Credentials = System.Net.CredentialCache.DefaultCredentials
                Dim Value = prox.GetValue(source)
                If IsNumeric(Value) Then
                    TableExists = True
                Else
                    TableExists = False
                End If
                prox.Dispose()
            Catch ex As Exception
                MsgBox(ex.Message)
                TableExists = False
            End Try
        End If
    End Function

    Public Shared Function GetBytes(ByVal Connection As String, ByVal source As String) As Byte()
        GetBytes = Nothing
        If Len(Connection) Then
            Dim cn As New SqlConnection(Connection)
            Try
                cn.Open()
                Dim co As New SqlCommand(source, cn)
                Dim dr As SqlDataReader = co.ExecuteReader(CommandBehavior.CloseConnection)
                Do While dr.Read
                    GetBytes = dr(0)
                Loop
                dr.Close()
                co.Dispose()
                cn.Dispose()
            Catch ex As Exception
                GetBytes = Nothing
                If cn.State = ConnectionState.Open Then cn.Dispose()
            End Try
        Else
            Dim prox As New PropidexLite.WebData.Service
            prox.PreAuthenticate = True
            prox.Credentials = System.Net.CredentialCache.DefaultCredentials
            Try
                GetBytes = prox.GetBytes(source)
            Catch ex As Exception
                GetBytes = Nothing
            End Try
            prox.Dispose()
        End If
    End Function

    Public Shared Function GetTable(ByVal Connection As String, ByVal source As String) As DataTable
        Dim DT As New DataTable
        Dim EM As String = ""
        Connection = Convert.ToString(Connection)
        If Len(Connection.ToString) Then
            Dim cn As New SqlConnection(Connection)
            Try
                Dim da As New SqlDataAdapter(source, cn)
                da.SelectCommand.CommandTimeout = 300
                da.Fill(DT)
                da.Dispose()
                cn.Dispose()
            Catch ex As System.Exception
                If cn.State = ConnectionState.Open Then cn.Dispose()
            End Try
        Else
            Dim prox As New PropidexLite.WebData.Service
            prox.PreAuthenticate = True
            prox.Credentials = System.Net.CredentialCache.DefaultCredentials
            Try
                DT = prox.GetTables(source, EM).Tables(0)
                If Len(EM) Then MsgBox(EM)
            Catch ex As Exception
                MsgBox(ex.Message)
            End Try
            prox.Dispose()
        End If
        Return DT

    End Function

    Public Shared Function GetTableEx(ByVal Connection As String, ByVal source As String, ByVal param As String) As DataTable
        Dim DT As New DataTable
        Connection = Connection.ToString()
        If Len(Connection) Then
            Dim cn As New SqlConnection(Connection)
            Try
                cn.Open()
                Dim da As New SqlDataAdapter(source, cn)
                Dim p() As String = Split(param, ";")
                For i As Integer = 0 To UBound(p)
                    Dim c() As String = Split(p(i), "=")
                    da.SelectCommand.Parameters.AddWithValue(c(0), c(1))
                Next
                da.SelectCommand.CommandTimeout = 300
                da.Fill(DT)
                da.Dispose()
                cn.Close()
                cn.Dispose()
            Catch ex As System.Exception
                MsgBox(ex.Message)
                If cn.State = ConnectionState.Open Then cn.Dispose()
            End Try
        Else
            Dim prox As New PropidexLite.WebData.Service
            prox.PreAuthenticate = True
            prox.Credentials = System.Net.CredentialCache.DefaultCredentials
            Try
                DT = prox.GetTablesEx(source, param).Tables(0)
            Catch ex As Exception
                MsgBox(ex.Message)
            End Try
            prox.Dispose()
        End If
        Return DT
    End Function
    Public Shared Function GetTables(ByVal Connection As String, ByVal Source As String) As DataSet
        Dim DS As New DataSet
        Dim EM As String = ""
        Connection = Connection.ToString()
        If Len(Connection) Then
            Dim cn As New SqlConnection(Connection)
            Try
                cn.Open()
                Dim da As New SqlDataAdapter(Source, cn)
                da.SelectCommand.CommandTimeout = 300
                da.Fill(DS)
                cn.Close()
                cn.Dispose()
            Catch ex As System.Exception
                MsgBox(ex.Message)
                If cn.State = ConnectionState.Open Then cn.Dispose()
            End Try
        Else
            Dim prox As New PropidexLite.WebData.Service
            prox.PreAuthenticate = True
            prox.Credentials = System.Net.CredentialCache.DefaultCredentials
            Try
                DS = prox.GetTables(Source, EM)
                If Len(EM) Then MsgBox(EM)
            Catch ex As Exception
                MsgBox(ex.ToString)
            End Try
            prox.Dispose()
        End If
        Return DS
    End Function
    Public Shared Function GetValue(ByVal Connection As String, ByVal source As String) As Object
        If Len(Connection) Then
            Dim cn As New SqlConnection(Connection)
            Try
                cn.Open()
                Dim co As New SqlCommand(source, cn)
                GetValue = co.ExecuteScalar
                co.Dispose()
                cn.Dispose()
            Catch ex As Exception
                GetValue = Nothing
                If cn.State = ConnectionState.Open Then cn.Dispose()
            End Try
        Else
            Try
                Dim prox As New PropidexLite.WebData.Service
                prox.PreAuthenticate = True
                prox.Credentials = System.Net.CredentialCache.DefaultCredentials
                GetValue = prox.GetValue(source)
                prox.Dispose()
            Catch ex As Exception
                MsgBox(ex.Message)
                GetValue = Nothing
            End Try
        End If
    End Function

    Public Shared Function GetValueEx(ByVal Connection As String, ByVal source As String, ByVal param As String) As Object
        If Len(Connection) Then
            Dim cn As New SqlConnection(Connection)
            Try
                cn.Open()
                Dim co As New SqlCommand(source, cn)
                Dim p() As String = Split(param, ";")
                For i As Integer = 0 To UBound(p)
                    Dim c() As String = Split(p(i), "=")
                    co.Parameters.AddWithValue(c(0), c(1))
                Next
                GetValueEx = co.ExecuteScalar
                co.Dispose()
                cn.Dispose()
            Catch ex As Exception
                GetValueEx = Nothing
                If cn.State = ConnectionState.Open Then cn.Dispose()
            End Try
        Else
            Try
                Dim prox As New PropidexLite.WebData.Service
                prox.PreAuthenticate = True
                prox.Credentials = System.Net.CredentialCache.DefaultCredentials
                GetValueEx = prox.GetValueEx(source, param)
                prox.Dispose()
            Catch ex As Exception
                MsgBox(ex.Message)
                GetValueEx = Nothing
            End Try
        End If
    End Function

    Public Shared Function GetTablesEx(ByVal Connection As String, ByVal Source As String, ByVal Param As String) As DataSet
        Dim ds As New DataSet
        Connection = Connection.ToString()
        If Len(Connection) Then
            Dim cn As New SqlConnection(Connection)
            Try
                cn.Open()
                Dim da As New SqlDataAdapter(Source, cn)
                Dim p() As String = Split(Param, ";")
                For i As Integer = 0 To UBound(p)
                    Dim c() As String = Split(p(i), "=")
                    da.SelectCommand.Parameters.AddWithValue(c(0), c(1))
                Next
                da.SelectCommand.CommandTimeout = 300
                da.Fill(ds)
                cn.Close()
                cn.Dispose()
            Catch ex As System.Exception
                MsgBox(ex.Message)
                If cn.State = ConnectionState.Open Then cn.Dispose()
            End Try
        Else
            Dim prox As New PropidexLite.WebData.Service
            prox.PreAuthenticate = True
            prox.Credentials = System.Net.CredentialCache.DefaultCredentials
            Try
                ds = prox.GetTablesEx(Source, Param)
            Catch ex As Exception
            End Try
            prox.Dispose()
        End If
        Return ds
    End Function
    Public Shared Function CvDate(ByVal yyyyMMdd As Object) As Date
        CvDate = Nothing
        Try
            Dim TF As String = Trim(CStr(yyyyMMdd))
            If Len(TF) = 8 Then
                Dim year As Integer = CInt(Mid(TF, 1, 4))
                Dim month As Integer = CInt(Mid(TF, 5, 2))
                Dim day As Integer = CInt(Mid(TF, 7, 2))
                CvDate = DateSerial(year, month, day)
            Else
                If IsDate(TF) Then CvDate = DateValue(TF)
            End If
        Catch ex As Exception
        End Try
    End Function
    Public Shared Function Apostrify(ByVal Buffer As String) As String
        Apostrify = ""
        If Len(Buffer) Then Apostrify = Buffer.Replace("'", "''")
    End Function

    Public Shared Function getConnection() As String
        Dim Connection = ""
        Dim Server As String = My.Settings.Server
        Dim DatabaseName As String = My.Settings.DatabaseName
        Dim DatabaseUID As String = My.Settings.DatabaseUID
        Dim DatabasePWD As String = My.Settings.DatabasePWD
        If Len(Trim(DatabaseName)) > 0 Then Connection = "Data Source=" & Server & ";User ID=" & DatabaseUID & ";PWD=" & DatabasePWD & ";Initial Catalog=" & DatabaseName & ";Connect Timeout=30"
        Return Connection
    End Function


    Public Shared Function Units(ByVal Connection As String, ByVal PNo As String, ByVal TF As String, ByVal LEX As String) As DataTable
        Dim vacDef As String = "M"
        Units = Nothing
        Dim SQL As String = "SELECT a.PNo, a.FNo, a.UNo, a.SType, a.U1 AS Start, b.*, c.FName, c.Elevation, c.Bank, d.PName, d.PAddress, e.AVC, e.LVL, e.AL1, e.ASF, e.MSF, e.AskRent, e.AskTerm, e.Comments, e.Language, e.StackNote, f.Description AS Availability, g.*, h.* FROM Units AS a INNER JOIN " & LEX & "UnitTypes as b ON a.UType=b.UType INNER JOIN Floors AS c ON a.PNo=c.PNo AND a.FNo=c.FNo INNER JOIN Properties AS d ON a.PNo=d.PNo LEFT JOIN Availability AS e ON a.PNo=e.PNo AND a.UNo=e.UN1 LEFT JOIN " & LEX & "Availtypes AS f ON e.AVC=f.AVC LEFT JOIN " & LEX & "UnitSpaceTypes AS g ON a.SType=g.SType LEFT JOIN UnitSub h ON a.PNo=h.PNo AND a.UNo=h.UNo WHERE (a.PNo=@PNO) AND (a.U1 <= @TF) AND (a.U2 >= @TF OR a.U2 Is Null) AND (e.LVL=1 OR e.LVL Is Null) ORDER BY c.Elevation, a.UNo"
        Dim PRM As String = "@PNO=" & PNo & ";@TF=" & TF
        Units = GetTableEx(Connection, SQL, PRM)

        Units.Columns.Add("A1", GetType(Integer))
        Units.Columns.Add("A2", GetType(Integer))
        Units.Columns.Add("Usable", GetType(Integer))
        Units.Columns.Add("Rentable", GetType(Integer))
        Units.Columns.Add("Billable", GetType(Integer))
        Units.Columns.Add("NSF", GetType(Single))
        Units.Columns.Add("PRA", GetType(Single))
        Units.Columns.Add("CRA", GetType(Single))
        Units.Columns.Add("RU", GetType(Single))
        Units.Columns.Add("RN", GetType(Single))
        Units.Columns.Add("RP", GetType(Single))
        Units.Columns.Add("U1", GetType(String))
        Units.Columns.Add("U2", GetType(String))
        Units.Columns.Add("U3", GetType(String))
        Units.Columns.Add("U4", GetType(String))
        Units.Columns.Add("U5", GetType(String))
        Units.Columns.Add("U6", GetType(String))
        Units.Columns.Add("U7", GetType(String))
        Units.Columns.Add("U8", GetType(String))
        Units.Columns.Add("U9", GetType(String))
        Units.Columns.Add("BUT", GetType(String))
        Units.Columns("BUT").MaxLength = 50
        Units.Columns.Add("BDS", GetType(String))
        Units.Columns("BDS").MaxLength = 50
        Units.Columns.Add("BDT", GetType(String))
        Units.Columns("BDT").MaxLength = 50
        Units.Columns.Add("BLT", GetType(String))
        Units.Columns("BLT").MaxLength = 50
        Units.Columns.Add("CDT", GetType(String))
        Units.Columns("CDT").MaxLength = 50
        Units.Columns.Add("BBR", GetType(Single))
        Units.Columns.Add("N2", GetType(Integer))
        Units.Columns.Add("T2", GetType(String))
        Units.Columns("T2").MaxLength = 50
        Units.Columns.Add("B2", GetType(Single))
        Units.Columns.Add("TILN2", GetType(Integer))
        Units.Columns.Add("MoVac", GetType(Integer))
        Units.Columns.Add("MoOcc", GetType(Integer))
        Units.Columns.Add("OStat", GetType(String))
        Units.Columns.Add("FOP", GetType(String))
        Units.Columns.Add("GOP", GetType(String))
        Units.Columns.Add("OOP", GetType(String))
        'Units.Columns.Add("UOP", GetType(String))
        Units.Columns.Add("VOP", GetType(String))
        Units.Columns.Add("ZOP", GetType(String))
        Units.Columns.Add("Notes", GetType(Integer))

        'Find Notes for each Unit
        '  SQL = "SELECT *  FROM Notes N WHERE (N.PNo=@PNO) AND (N.UserId='" & Apostrify(Propidex.Common.UserID) & "' Or N.Shared=1)"
        '  Dim Notes As DataTable = GetTableEx(Connection, SQL, PRM)

        SQL = "SELECT * FROM UnitAreas WHERE (PNo=@PNO) AND (A1 <= @TF) AND (A2 >= @TF OR A2 IS NULL)"
        Dim Areas As DataTable = GetTableEx(Connection, SQL, PRM)
        Dim UR, AR, US() As DataRow
        For Each UR In Units.Rows
            US = Areas.Select("UNo='" & UR("UNo") & "'")
            If US.Length Then
                UR("Usable") = 0
                UR("Rentable") = 0
                UR("Billable") = 0
                For Each AR In US
                    If AR("Type") = "U" Then UR("Usable") = AR("Area")
                    If AR("Type") = "R" Then
                        UR("Rentable") = AR("Area")
                        UR("A1") = AR("A1")
                        UR("A2") = AR("A2")
                    End If
                    If AR("Type") = "B" Then UR("Billable") = AR("Area")
                Next
                If UR("Usable") Then UR("RU") = UR("Rentable") / UR("Usable")
            Else
                UR.Delete()
            End If
        Next
        Units.AcceptChanges()

        SQL = "SELECT Spaces.UNo, Spaces.NSF, Spaces.PRA, Spaces.CRA FROM Spaces INNER JOIN Maps ON Spaces.MNo=Maps.MNo WHERE (Maps.PNo=@PNO) AND (Maps.Type=0) AND (Maps.D1 <= @TF) AND (Maps.D2 >= @TF OR Maps.D2 IS NULL) AND (Spaces.CNo > 3)"
        Areas = GetTableEx(Connection, SQL, PRM)
        For Each UR In Units.Rows
            US = Areas.Select("UNo='" & UR("UNo") & "'")
            UR("NSF") = 0
            UR("PRA") = 0
            UR("CRA") = 0
            For Each AR In US
                If Not IsDBNull(AR("NSF")) Then UR("NSF") = AR("NSF")
                If Not IsDBNull(AR("PRA")) Then UR("PRA") = AR("PRA")
                If Not IsDBNull(AR("CRA")) Then UR("CRA") = AR("CRA")
                If UR("NSF") Then UR("RN") = UR("Rentable") / UR("NSF")
                If UR("PRA") Then UR("RP") = UR("Rentable") / UR("PRA")
            Next
            '  UR("Notes") = Notes.Select("UNo='" & UR("UNo") & "'").Length
        Next

        'MONTHS VACANT/OCCUPIED
        Dim VD1 As String = vacDef & "1"
        Dim VD2 As String = vacDef & "2"

        SQL = "SELECT a.*, b.TName, c.Vacant FROM Leases a INNER JOIN Tenants b ON a.TNo=b.TNo INNER JOIN " & LEX & "LeaseTypes c ON a.LType = c.LType WHERE PNo=@PNO ORDER BY " & VD1 & " DESC"
        Areas = GetTableEx(Connection, SQL, PRM)

        SQL = "SELECT a.* FROM Charges a INNER JOIN " & LEX & "ChargeCodes AS b ON a.ACode = b.ChargeCode INNER JOIN stdChargeCodes AS c ON b.stdChargeCode = c.ChargeCode WHERE (a.PNo=@PNO) AND (a.Freq='A' Or a.Freq='S' Or a.Freq='Q' Or a.Freq='M') AND (c.ChargeCodeGrouping='B' OR c.ChargeCodeGrouping='G') ORDER BY UNo"
        Dim Bases As DataTable = GetTableEx(Connection, SQL, PRM)

        Dim V As String = Resource("Portfolio", "Vacant", "Vacant")
        Dim A As String = Resource("Portfolio", "Active", "Active")
        Dim F As String = Resource("Portfolio", "Future", "Future")

        'Need to figure out Actions
        '  Dim LeaseFilter As String = TypeFilter
        Dim LeaseFilter As String = ""

        For Each UR In Units.Rows
            If Len(LeaseFilter) Then
                Try
                    US = Areas.Select("UNo='" & UR("UNo") & "' AND " & LeaseFilter)
                Catch ex As Exception
                    MsgBox(ex.Message)
                    LeaseFilter = ""
                    US = Areas.Select("UNo='" & UR("UNo") & "'")
                End Try
            Else
                US = Areas.Select("UNo='" & UR("UNo") & "'")
            End If
            If US.Length = 0 Then
                UR("MoVac") = DateDiff(DateInterval.Month, CvDate(UR("Start")), Today)
                UR("OStat") = V
            Else
                UR("N2") = US(0)(VD2)
                UR("T2") = US(0)("TName") & ""
                Dim DD As Integer = 20991231
                If IsDBNull(UR("N2")) Then
                    If Not IsDBNull(US(0)("D2")) Then DD = US(0)("D2")
                Else
                    DD = UR("N2")
                    UR("TILN2") = DateDiff(DateInterval.Day, CvDate(TF), DateValue(CvDate(DD))) + 1
                End If
                Dim ULNo = US(0)("LNo")
                Dim YR As Single
                Dim BR As Single = 0
                Dim BB() As DataRow = Bases.Select("UNo='" & UR("UNo") & "'")
                For Each B As DataRow In BB
                    If B("LNo") = ULNo Then
                        If B("AD1") <= DD And (B("AD2") >= DD Or IsDBNull(B("AD2"))) Then
                            If B("Amount") Then
                                If B("Freq") = "M" Then
                                    YR = B("Charge") * 12
                                ElseIf B("Freq") = "A" Then
                                    YR = B("Charge")
                                ElseIf B("Freq") = "S" Then
                                    YR = B("Charge") * 2
                                ElseIf B("Freq") = "Q" Then
                                    YR = B("Charge") * 4
                                End If
                                BR += YR / B("Amount")
                            End If
                        End If
                    End If
                Next
                UR("B2") = BR
                ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
                For N As Integer = 0 To US.Length - 1
                    AR = US(N)
                    If CvDate(AR(VD1)) > Today Then
                        UR("OStat") = F
                    Else
                        If IsDBNull(AR(VD2)) Then
                            UR("MoOcc") = DateDiff(DateInterval.Month, CvDate(AR(VD1)), Today)
                            UR("OStat") = A
                        Else
                            If CvDate(AR(VD2)) > Today Then
                                UR("MoOcc") = DateDiff(DateInterval.Month, CvDate(AR(VD1)), Today)
                                UR("OStat") = A
                            Else
                                UR("MoVac") = DateDiff(DateInterval.Month, CvDate(AR(VD2)), Today)
                                If N = 0 Then UR("OStat") = V
                            End If
                            Exit For
                        End If
                    End If
                Next
            End If
        Next

        'UNITDATA
        SQL = "SELECT UNo, Name, Value FROM UnitData WHERE (PNo=@PNO) AND (D1<=@TF) AND (D2>=@TF OR D2 IS Null)"
        Areas = GetTableEx(Connection, SQL, PRM)
        For Each UR In Units.Rows
            US = Areas.Select("UNo='" & UR("UNo") & "'")
            For Each AR In US
                Try
                    UR(AR("Name")) = AR("Value")
                Catch ex As Exception
                End Try
            Next
        Next

        'UNITOPTIONS
        SQL = "SELECT a.OptionUnit AS UNo, c.OptionCodeGrouping AS OP FROM LeaseLog AS a INNER JOIN " & LEX & "LeaseOptionCodes AS b ON a.LeaseOptionCode=b.LeaseOptionCode INNER JOIN stdLeaseOptionCodes AS c ON b.stdLeaseOptionCode=c.LeaseOptionCode INNER JOIN Leases AS d ON a.LNo=d.LNo WHERE (a.OptionProperty=@PNO) AND ((CONVERT(datetime,a.[LeaseOptionEndDate]) >= CONVERT(datetime,@TF)) OR (a.[LeaseOptionEndDate] Is Null)) AND (c.Encumbrance=1) AND (d.M2 > @TF OR d.M2 Is Null)"
        PRM = "@PNO=" & PNo & ";@TF=" & TF
        Areas = GetTableEx(Connection, SQL, PRM)

        For Each UR In Units.Rows
            UR("FOP") = ""
            UR("GOP") = ""
            UR("OOP") = ""
            'UR("UOP") = ""
            UR("VOP") = ""
            UR("ZOP") = ""
            US = Areas.Select("UNo='" & UR("UNo") & "'")
            If US.Length Then
                UR("ZOP") = "Y"
                For Each AR In US
                    If Not IsDBNull(AR("OP")) Then
                        If (AR("OP")) = "F" Then UR("FOP") = "Y"
                        If (AR("OP")) = "G" Then UR("GOP") = "Y"
                        If (AR("OP")) = "O" Then UR("OOP") = "Y"
                        If (AR("OP")) = "V" Then UR("VOP") = "Y"
                    End If
                Next
            End If
        Next

        'Removed Budgets

        Units.AcceptChanges()

    End Function

    Public Shared Function Tenants(ByVal Connection As String, ByVal PNo As String, ByVal TF As String, ByVal LEX As String) As DataTable
        Tenants = Nothing
        Dim vacDef As String = "M"
        Try
            Dim VD1 As String = vacDef & "1"
            Dim VD2 As String = vacDef & "2"
            Dim Options As String = "SELECT a.LNo, c.OptionCodeGrouping AS OP FROM LeaseLog AS a INNER JOIN " & LEX & "LeaseOptionCodes AS b ON a.LeaseOptionCode=b.LeaseOptionCode INNER JOIN stdLeaseOptionCodes AS c ON b.stdLeaseOptionCode = c.LeaseOptionCode WHERE (a.PNo=@PNO) AND (CONVERT(datetime,a.LeaseOptionEndDate) >= CONVERT(datetime,@TF) OR (a.LeaseOptionEndDate Is Null))"

            Dim Rents As String = "SELECT a.LNo, a.UNo, b.Freq, b.Amount, Sum(b.Charge) AS Rent, d.ChargeCodeGrouping AS Type FROM Leases AS a INNER JOIN Charges AS b ON a.LNo=b.LNo AND a.PNo=b.PNo AND a.UNo=b.UNo INNER JOIN " & LEX & "ChargeCodes AS c ON b.ACode = c.ChargeCode INNER JOIN stdChargeCodes AS d ON c.stdChargeCode = d.ChargeCode" _
                & " WHERE (a.PNo=@PNO) AND (b.Freq='A' Or b.Freq='S' Or b.Freq='Q' Or b.Freq='M') AND (b.AD1 <= @TF) AND (b.AD2 Is Null OR b.AD2 >= @TF)" _
                & " GROUP BY a.LNo, a.UNo, b.Freq, b.Amount, d.ChargeCodeGrouping ORDER BY a.UNo"

            Dim Source As String = "SELECT a.*, b.*, c.Description AS LeaseStatus, d.Description AS LeaseType, d.Vacant, e.SICDescription, f.CName AS Category, g.GName AS [Group], h.PNo, h.FNo, h.UNo, i.TName, i.Address1, i.Address2, i.City, i.Region, i.PostalCode, i.Phone, j.*" _
                & " FROM Leases AS a LEFT JOIN Breakpoints AS b ON a.LNo=b.LNo AND a.PNo=b.PNo AND a.UNo=b.UNo LEFT JOIN " & LEX & "LeaseStatus AS c ON a.LStat = c.LStat LEFT JOIN " & LEX & "LeaseTypes AS d ON a.LType = d.LType LEFT JOIN " & LEX & "SICCodes AS e ON a.SIC=e.SIC LEFT JOIN " & LEX & "Categories AS f ON a.CCode=f.CCode LEFT JOIN " & LEX & "Groups AS g ON f.[Group]=g.[Group] LEFT JOIN Tenants AS i ON a.TNo=i.TNo INNER JOIN Units AS h ON a.PNo=h.PNo AND a.UNo=h.UNo LEFT JOIN LeaseSub j ON a.PNo=j.PNo AND a.UNo=j.UNo AND a.LNo=j.LNo" _
                & " WHERE (a.PNo=@PNO) AND (a." & VD1 & " <= @TF) AND (a." & VD2 & " >= @TF OR a." & VD2 & " IS NULL)" _
                & "           UNION SELECT a.*, b.*, c.Description AS LeaseStatus, d.Description AS LeaseType, d.Vacant, e.SICDescription, f.CName AS Category, g.GName AS [Group], h.PNo, h.FNo, h.UNo, i.TName, i.Address1, i.Address2, i.City, i.Region, i.PostalCode, i.Phone, j.*" _
                & " FROM Leases AS a LEFT JOIN Breakpoints AS b ON a.LNo=b.LNo AND a.PNo=b.PNo AND a.UNo=b.UNo LEFT JOIN " & LEX & "LeaseStatus AS c ON a.LStat = c.LStat LEFT JOIN " & LEX & "LeaseTypes AS d ON a.LType = d.LType LEFT JOIN " & LEX & "SICCodes AS e ON a.SIC=e.SIC LEFT JOIN " & LEX & "Categories AS f ON a.CCode=f.CCode LEFT JOIN " & LEX & "Groups AS g ON f.[Group]=g.[Group] LEFT JOIN Tenants AS i ON a.TNo=i.TNo INNER JOIN Units AS h ON a.PNo=h.PNo AND a.UNo=h.UNo LEFT JOIN LeaseSub j ON a.PNo=j.PNo AND a.UNo=j.UNo AND a.LNo=j.LNo" _
                & " WHERE (a.PNo=@PNO) AND (a." & VD1 & " > @TF)"

            Dim Subs As String = "SELECT ParentLNo, MasterUNo, LNo AS SubLease, SubUNo, Tenant, D2, RSF FROM SubTenants WHERE (PNo=@PNO) AND (M1<= @TF) AND (M2>= @TF OR M2 IS NULL) GROUP BY ParentLNo, MasterUNo, LNo, SubUNo, Tenant, D2, RSF"

            Dim SQL As String = Options & ";" & Rents & ";" & Source & ";" & Subs
            Dim PRM As String = "@PNO=" & PNo & ";@TF=" & TF
            Dim t1 As DataTable = GetTableEx(Connection, Options, PRM)
            Dim t2 As DataTable = GetTableEx(Connection, Rents, PRM)
            Dim t3 As DataTable = GetTableEx(Connection, Source, PRM)
            Dim t4 As DataTable = GetTableEx(Connection, Subs, PRM)

            Dim DSS As DataSet = GetTablesEx(Connection, SQL, PRM)
            Dim OS As DataTable = DSS.Tables(0)
            Dim RT As DataTable = DSS.Tables(1)

            Tenants = DSS.Tables(2)
            Tenants.Columns.Add("TILD2", GetType(Integer))
            Tenants.Columns.Add("TILM2", GetType(Integer))
            Tenants.Columns.Add("TILL2", GetType(Integer))

            Tenants.Columns.Add("A", GetType(Single))
            Tenants.Columns.Add("G", GetType(Single))    'Additional Base Rent
            Tenants.Columns.Add("B", GetType(Single))
            Tenants.Columns.Add("B2", GetType(Single))   'Last Step
            Tenants.Columns.Add("C", GetType(Single))
            Tenants.Columns.Add("P", GetType(Single))
            Tenants.Columns.Add("T", GetType(Single))
            Tenants.Columns.Add("X", GetType(Single))
            Tenants.Columns.Add("Base", GetType(Single)) 'SubTotal
            Tenants.Columns.Add("BaseM", GetType(Single))
            Tenants.Columns.Add("AM", GetType(Single))
            Tenants.Columns.Add("GM", GetType(Single))
            Tenants.Columns.Add("BM", GetType(Single))
            Tenants.Columns.Add("CM", GetType(Single))
            Tenants.Columns.Add("PM", GetType(Single))
            Tenants.Columns.Add("TM", GetType(Single))
            Tenants.Columns.Add("XM", GetType(Single))
            Tenants.Columns.Add("Total", GetType(Single))
            Tenants.Columns.Add("PerMonth", GetType(Single))
            Tenants.Columns.Add("Annual", GetType(Single))
            Tenants.Columns.Add("AnnualBase", GetType(Single))
            Tenants.Columns.Add("Monthly", GetType(Single))
            Tenants.Columns.Add("MonthlyBase", GetType(Single))

            Tenants.Columns.Add("RS", GetType(Single))

            Tenants.Columns.Add("DOP", GetType(String))
            Tenants.Columns.Add("HOP", GetType(String))
            Tenants.Columns.Add("IOP", GetType(String))
            Tenants.Columns.Add("ROP", GetType(String))
            Tenants.Columns.Add("SOP", GetType(String))
            Tenants.Columns.Add("TOP", GetType(String))
            Tenants.Columns.Add("XOP", GetType(String))

            Tenants.Columns.Add("FOP", GetType(String))
            Tenants.Columns.Add("GOP", GetType(String))
            Tenants.Columns.Add("OOP", GetType(String))
            Tenants.Columns.Add("VOP", GetType(String))

            Dim ST As DataTable = DSS.Tables(3)
            Tenants.Columns.Add("SubTenant", GetType(String))
            Tenants.Columns.Add("SubExpiry", GetType(String))
            Tenants.Columns.Add("SubArea", GetType(String))

            SQL = "SELECT a.* FROM Charges a INNER JOIN " & LEX & "ChargeCodes b ON a.ACode = b.ChargeCode INNER JOIN stdChargeCodes c ON b.stdChargeCode = c.ChargeCode WHERE (a.PNo='" & PNo & "') AND (a.Freq='A' Or a.Freq='S' Or a.Freq='Q' Or a.Freq='M') AND (c.ChargeCodeGrouping='B' OR c.ChargeCodeGrouping='G') ORDER BY UNo"
            Dim Bases As DataTable = GetTable(Connection, SQL)

            For Each TR As DataRow In Tenants.Rows
                Dim DD As Integer = 20991231
                If IsDBNull(TR(VD2)) Then
                    If Not IsDBNull(TR("D2")) Then DD = TR("D2")
                Else
                    DD = TR(VD2)
                End If
                Dim AR As Single
                Dim BR As Single = 0
                Dim BB() As DataRow = Bases.Select("LNo='" & TR("LNo") & "'")
                For Each B As DataRow In BB
                    If B("UNo") = TR("UNo") Then
                        If B("AD1") <= DD And (B("AD2") >= DD Or IsDBNull(B("AD2"))) Then
                            If B("Amount") Then
                                If B("Freq") = "M" Then
                                    AR = B("Charge") * 12
                                ElseIf B("Freq") = "A" Then
                                    AR = B("Charge")
                                ElseIf B("Freq") = "S" Then
                                    AR = B("Charge") * 2
                                ElseIf B("Freq") = "Q" Then
                                    AR = B("Charge") * 4
                                End If
                                BR += AR / B("Amount")
                            End If
                        End If
                    End If
                Next
                TR("B2") = BR
                If Not IsDBNull(TR("D2")) Then TR("TILD2") = DateDiff(DateInterval.Day, CvDate(TF), DateValue(CvDate(TR("D2")))) + 1
                If Not IsDBNull(TR("M2")) Then TR("TILM2") = DateDiff(DateInterval.Day, CvDate(TF), DateValue(CvDate(TR("M2")))) + 1
                If Not IsDBNull(TR("L2")) Then TR("TILL2") = DateDiff(DateInterval.Day, CvDate(TF), DateValue(CvDate(TR("L2")))) + 1
                TR("A") = 0
                TR("G") = 0
                TR("B") = 0
                TR("C") = 0
                TR("P") = 0
                TR("T") = 0
                TR("X") = 0
                TR("AM") = 0
                TR("GM") = 0
                TR("BM") = 0
                TR("CM") = 0
                TR("PM") = 0
                TR("TM") = 0
                TR("XM") = 0
                TR("Base") = 0
                TR("BaseM") = 0
                TR("Total") = 0
                TR("PerMonth") = 0
                TR("Annual") = 0
                TR("AnnualBase") = 0
                TR("Monthly") = 0
                TR("MonthlyBase") = 0
                TR("RS") = 0

                TR("DOP") = ""
                TR("HOP") = ""
                TR("IOP") = ""
                TR("ROP") = ""
                TR("SOP") = ""
                TR("TOP") = ""
                TR("XOP") = ""

                TR("FOP") = ""
                TR("GOP") = ""
                TR("OOP") = ""
                TR("VOP") = ""

                Dim RR() As DataRow = RT.Select("LNo='" & Apostrify(TR("LNo")) & "' AND UNo='" & TR("UNo") & "'")
                Dim PF, Base, Total, Annual As Single
                AR = 0
                PF = 0
                Base = 0
                Total = 0
                Annual = 0
                For Each R As DataRow In RR
                    If R("Freq") = "M" Then
                        AR = R("Rent") * 12
                    ElseIf R("Freq") = "A" Then
                        AR = R("Rent")
                    ElseIf R("Freq") = "S" Then
                        AR = R("Rent") * 2
                    ElseIf R("Freq") = "Q" Then
                        AR = R("Rent") * 4
                    End If
                    Select Case Trim(R("Type"))
                        Case "A" 'ADDITIONAL
                            Annual = Annual + AR
                            If R("Amount") Then
                                PF = AR / R("Amount")
                                TR("A") = TR("A") + PF
                                Total = Total + PF
                            End If
                        Case "G" 'ADDITIONAL BASE
                            Annual = Annual + AR
                            TR("AnnualBase") = TR("AnnualBase") + AR
                            TR("MonthlyBase") = TR("MonthlyBase") + AR / 12
                            If R("Amount") Then
                                PF = AR / R("Amount")
                                TR("G") = TR("G") + PF
                                Base = Base + PF
                                Total = Total + PF
                            End If
                        Case "B" 'BASE
                            Annual = Annual + AR
                            TR("AnnualBase") = TR("AnnualBase") + AR
                            TR("MonthlyBase") = TR("MonthlyBase") + AR / 12
                            If R("Amount") Then
                                PF = AR / R("Amount")
                                TR("B") = TR("B") + PF
                                Base = Base + PF
                                Total = Total + PF
                            End If
                        Case "C" 'CAM
                            Annual = Annual + AR
                            If R("Amount") Then
                                PF = AR / R("Amount")
                                TR("C") = TR("C") + PF
                                Total = Total + PF
                            End If
                        Case "P" 'PERCENT RENT
                            Annual = Annual + AR
                            If R("Amount") Then
                                PF = AR / R("Amount")
                                TR("P") = TR("P") + PF
                                Total = Total + PF
                            End If
                        Case "T" 'TAXES
                            Annual = Annual + AR
                            If R("Amount") Then
                                PF = AR / R("Amount")
                                TR("T") = TR("T") + PF
                                Total = Total + PF
                            End If
                        Case "X" 'NOT INCLUDED
                            If R("Amount") Then
                                TR("X") = TR("X") + AR / R("Amount")
                            End If
                    End Select
                Next

                'total dollars

                TR("Annual") = Annual
                TR("Monthly") = Annual / 12

                'per foot

                TR("Base") = Base
                TR("Total") = Total
                TR("PerMonth") = Total / 12
                TR("AM") = TR("A") / 12
                TR("GM") = TR("G") / 12
                TR("BM") = TR("B") / 12
                TR("BaseM") = TR("GM") + TR("BM")
                TR("CM") = TR("C") / 12
                TR("PM") = TR("P") / 12
                TR("TM") = TR("T") / 12
                TR("XM") = TR("X") / 12

                If DateDiff(DateInterval.Month, CvDate(TR("M1")), Today) > 11 Then
                    If Not IsDBNull(TR("Sales")) Then
                        If TR("Sales") > 0 Then
                            TR("RS") = Annual * 100 / TR("Sales")
                        End If
                    End If
                End If

                RR = ST.Select("ParentLNo='" & Apostrify(TR("LNo")) & "' AND MasterUNo='" & TR("UNo") & "'")
                Dim SubTenant As String = ""
                Dim SubExpiry As String = ""
                Dim SubArea As String = ""
                For Each R As DataRow In RR
                    If Len(SubTenant) Then SubTenant &= " + "
                    SubTenant &= R("Tenant") & ""
                    If IsNumeric(R("D2")) Then
                        If Len(SubExpiry) Then SubExpiry &= " + "
                        SubExpiry &= CvDate(R("D2")).ToShortDateString
                    End If
                    If IsNumeric(R("RSF")) Then
                        If Len(SubArea) Then SubArea &= " + "
                        If UPSF = 1 Then
                            SubArea &= Format(R("RSF"), "#,###")
                        Else
                            SubArea &= Format(R("RSF") * UPSF, "#,###.0")
                        End If
                    End If
                Next
                If Len(SubTenant) Then TR("SubTenant") = SubTenant
                If Len(SubExpiry) Then TR("SubExpiry") = SubExpiry
                If Len(SubArea) Then TR("SubArea") = SubArea

                RR = OS.Select("LNo='" & Apostrify(TR("LNo")) & "'")
                For Each R As DataRow In RR
                    If Not IsDBNull(R("OP")) Then
                        If R("OP") = "D" Then TR("DOP") = "Y"
                        If R("OP") = "H" Then TR("HOP") = "Y"
                        If R("OP") = "I" Then TR("IOP") = "Y"
                        If R("OP") = "R" Then TR("ROP") = "Y"
                        If R("OP") = "S" Then TR("SOP") = "Y"
                        If R("OP") = "T" Then TR("TOP") = "Y"
                        If R("OP") = "E" Then TR("XOP") = "Y"

                        If R("OP") = "F" Then TR("FOP") = "Y"
                        If R("OP") = "G" Then TR("GOP") = "Y"
                        If R("OP") = "O" Then TR("OOP") = "Y"
                        If R("OP") = "V" Then TR("VOP") = "Y"
                    End If
                Next
            Next
            Tenants.AcceptChanges()
        Catch ex As Exception
            MsgBox(ex.Message, , "Tenants")
        End Try
    End Function

End Class
