Imports Microsoft.SmallBasic.Library
Imports System.Text.RegularExpressions
Imports iTextSharp.text.pdf
Imports iTextSharp.text.pdf.parser


Imports DotSpatial.Data
Imports DotSpatial.Topology
Imports System.IO


Module Module1

    Dim Version As String = "1.0"


    Dim parseResult As New List(Of obstacleStruct)
    Structure obstacleStruct
        Dim name As String
        Dim type As String
        Dim elevation As Short
        Dim lat As Double
        Dim lon As Double
        Dim marked As Boolean
        Dim lighted As Boolean
        Dim height As Short
    End Structure

    ' header
    Sub writeHeader()
        Console.BackgroundColor = ConsoleColor.Gray
        Console.ForegroundColor = ConsoleColor.Black

        Console.WriteLine("(c) neway data AG - Landstraße 105 - 9490 Vaduz - FL-0002.103.140-4")
        Console.WriteLine("AIP OBSTACLE IMPORTER" & "    VERSION: " & Version & " ")
        Console.ResetColor()
    End Sub

    Public Function ParsePdfText(ByVal sourcePDF As String,
                                  Optional ByVal fromPageNum As Integer = 0,
                                  Optional ByVal toPageNum As Integer = 0) As String

        Dim sb As New System.Text.StringBuilder()
        Try
            Dim reader As New PdfReader(sourcePDF)
            If fromPageNum = 0 Then
                fromPageNum = 1
            End If
            If toPageNum = 0 Then
                toPageNum = reader.NumberOfPages
            End If

            If fromPageNum > toPageNum Then
                Throw New ApplicationException("Parameter error: The value of fromPageNum can " &
                                           "not be larger than the value of toPageNum")
            End If

            Dim text As String = ""
            For i As Integer = fromPageNum To toPageNum Step 1
                text &= PdfTextExtractor.GetTextFromPage(reader, i)
            Next i

            processText(text)

        Catch ex As Exception

            Return String.Empty
        End Try
        Return sb.ToString()
    End Function

    Sub processText(rawtext As String)

        Dim pattern As String = "[NS] [0-8][0-9] [0-5][0-9] [0-9][0-9]"

        ' Instantiate the regular expression object.
        Dim r As Regex = New Regex(pattern, RegexOptions.IgnoreCase)

        Dim m As Match = r.Match(rawtext)
        Dim matchcount As Integer = 0
        Do While m.Success
            Dim targetAddress = m.Index
            Dim PreBufferLength As Short = 100
            Dim PostBufferLength As Short = 100
            If targetAddress < PreBufferLength Then PreBufferLength = targetAddress
            If rawtext.Length < targetAddress + PostBufferLength Then PostBufferLength = rawtext - targetAddress

            ' text before the found element
            Dim elementStringPre = rawtext.Substring(targetAddress - PreBufferLength, PreBufferLength)
            Dim elementSplitPre = elementStringPre.Split(" ").ToList

            elementSplitPre.Reverse()


            ' ========================
            ' get obstacle name
            ' ========================
            ' look until a new line is found
            Dim name_lst As New List(Of String)
            Dim name As String = ""
            For Each item In elementSplitPre
                If item <> "" Then
                    If item.Contains(vbLf) Then Exit For
                    name_lst.Add(item)
                End If
            Next

            ' invert the wording
            name_lst.Reverse()
            For Each item In name_lst
                name &= " " & item
            Next

            ' text after the found element
            Dim elementStringPost = rawtext.Substring(targetAddress, PostBufferLength)
            Dim elementSplitPost = elementStringPost.Split(" ").ToList



            ' ========================
            ' get obstacle type
            ' ========================
            Dim type_lst As New List(Of String)
            Dim type As String = ""
            Dim counter As Short = 0

            Dim matches As New List(Of String)
            For Each item In elementSplitPost
                matches.Add(item)
                ' find the first usefule word
                Dim p As String = "\b[A-Za-z]{4,}\b"

                ' Instantiate the regular expression object.
                Dim s As Regex = New Regex(p, RegexOptions.IgnoreCase)

                Dim q As Match = s.Match(item)

                ' use the second word found...
                If q.Success Then
                    Dim definition = checkWord(item)
                    If definition Then
                        counter += 1
                        ' A string is returned if the word exists in the dictionary
                        'Console.WriteLine(CStr(definition))

                        type_lst.Add(item)
                        If counter > 5 Then Exit For
                    End If
                End If
            Next



            For Each t In type_lst
                type &= " " & t
            Next

            ' ========================
            ' latitude
            ' ========================
            Dim patternLat As String = "[NS] [0-8][0-9] [0-5][0-9] [0-9][0-9]"

            ' Instantiate the regular expression object.
            Dim latex As Regex = New Regex(patternLat, RegexOptions.IgnoreCase)
            Dim latMatch As Match = latex.Match(elementStringPost)

            Dim latitudeString As String = latMatch.Value

            Dim latitude As Double = lat2double(latitudeString)

            ' ========================
            ' longitude
            ' ========================
            Dim patternLon As String = "[EW] [0-3][0-8][0-9] [0-5][0-9] [0-9][0-9]"



            ' Instantiate the regular expression object.
            Dim lonex As Regex = New Regex(patternLon, RegexOptions.IgnoreCase)
            Dim lonMatch As Match = lonex.Match(elementStringPost)

            Dim longitudeString As String = lonMatch.Value

            Dim longitude As Double = lon2double(longitudeString)


            Try
                ' parse Lat/Lon
                ' in DFS Germany always N E
                If latitudeString.Contains("N") = False Then
                    Console.Write("ERR: latitude not in northern Hemisphere! " & latitudeString)
                End If
                If longitudeString.Contains("E") = False Then
                    Console.Write("ERR: latitude not in eastern Hemisphere! " & longitudeString)
                End If
            Catch ex As Exception
            End Try

            If type = "" Then
                Console.ForegroundColor = ConsoleColor.Yellow
                Console.WriteLine("WARN: not found type! " & name)
                Console.ResetColor()
            End If

            ' ========================
            ' elevation  / height
            ' ========================
            Dim elevation As Double = 0
            Dim height As Double = 0
            Dim state As Short = 0
            Dim patternElev As String = "([1-9][0-9][0-9][0-9]|[1-9][0-9][0-9])"
            For Each item In elementSplitPost
                ' Instantiate the regular expression object.
                Dim elevex As Regex = New Regex(patternElev, RegexOptions.IgnoreCase)
                Dim elevMatch As Match = elevex.Match(item)

                If elevMatch.Success Then
                    If state = 0 Then
                        elevation = elevMatch.Value
                        state = 1
                    Else
                        height = elevMatch.Value
                        Exit For
                    End If
                End If
            Next



            If elevation = 0 Or height = 0 Then
                Console.ForegroundColor = ConsoleColor.Yellow
                Console.WriteLine("WARN: elevation or height not found! " & name)
                Console.ResetColor()
            End If


            ' ========================
            ' lighted  / marked
            ' ========================
            Dim lighted As Boolean = False
            Dim marked As Boolean = False
            Dim state2 As Short = 0
            For Each item In elementSplitPost
                If item.Contains("yes") Or item.Contains("no") Then
                    If state2 = 0 Then
                        If item.Contains("yes") Then
                            lighted = True
                        End If
                        state2 = 1
                    Else
                        If item.Contains("yes") Then
                            marked = True
                            Exit For
                        End If
                    End If
                End If
            Next

            Console.WriteLine((name.PadLeft(40)) & "-> " & type.PadLeft(30) & " -> " & latitudeString & " | " & longitudeString & " -> elev: " & elevation.ToString.PadLeft(5) & " -> height: " & height.ToString.PadLeft(5) & " ->  lighted: " & lighted & " -> marked: " & marked)

            Dim newObs As obstacleStruct
            newObs.name = name
            newObs.type = type
            newObs.lat = latitude
            newObs.lon = longitude
            newObs.height = height
            newObs.elevation = elevation
            newObs.marked = marked
            newObs.lighted = lighted
            parseResult.Add(newObs)





            ' text after the found element

            m = m.NextMatch()

            '  Console.ReadKey()
        Loop



    End Sub

    Dim wordList As New List(Of String)
    Dim notFound As New List(Of String)
    Function checkWord(item As String)

        If notFound.Contains(item) Then Return False

        If wordList.Contains(item) Then
            Return True
        Else

            If CStr(Dictionary.GetDefinition(item)) <> "" Then
                wordList.Add(item)
                Return True
            Else
                notFound.Add(item)
                Return False
            End If
        End If
        Return False
    End Function

    Function lat2double(str As String) As Double
        Try

            Dim sp = str.Split(" ")
            Dim degr As Double = sp(1) + sp(2) / 60 + sp(3) / 3600
            Return degr
        Catch ex As Exception
            Console.WriteLine("ERR: cant recognize latitude! " & str)
        End Try
        Return Nothing
    End Function

    Function lon2double(str As String) As Double
        Try
            Dim sp = str.Split(" ")
            Dim degr As Double = sp(1) + sp(2) / 60 + sp(3) / 3600

            Return degr
        Catch ex As Exception
            Console.WriteLine("ERR: cant recognize longitude! " & str)
        End Try
        Return Nothing
    End Function



    Sub Main()

        writeHeader()


        Try
            Dim fileEntries As String() = Directory.GetFiles("aipDocs")
            ' Process the list of files found in the directory.
            Dim fileName As String
            For Each fileName In fileEntries
                If fileName.EndsWith(".pdf") Then

                    Console.WriteLine("process file: " & fileName)
                    Console.WriteLine("-------------------------------------------------->")
                    ParsePdfText(fileName)
                End If


            Next fileName

            createShapefile()
        Catch ex As Exception
            Console.WriteLine("ERR: " & ex.Message)
        End Try

    End Sub


    Sub createShapefile()
        ' create out folder
        If System.IO.Directory.Exists("\out") = False Then
            System.IO.Directory.CreateDirectory("\out")
        End If

        Dim fs As New FeatureSet(FeatureType.Line)
        fs.DataTable.Columns.Add(New DataColumn("LFNDNR", Type.GetType("System.Int32")))
        fs.DataTable.Columns.Add(New DataColumn("GZ", Type.GetType("System.Int32")))
        fs.DataTable.Columns.Add(New DataColumn("ART", Type.GetType("System.String")))
        fs.DataTable.Columns.Add(New DataColumn("OBJNAME", Type.GetType("System.String")))
        fs.DataTable.Columns.Add(New DataColumn("BUNDESLAND", Type.GetType("System.String")))
        fs.DataTable.Columns.Add(New DataColumn("LAGE", Type.GetType("System.String")))
        fs.DataTable.Columns.Add(New DataColumn("H_MAX", Type.GetType("System.Int32")))
        fs.DataTable.Columns.Add(New DataColumn("H_TAL", Type.GetType("System.Int32")))
        fs.DataTable.Columns.Add(New DataColumn("N_BERG", Type.GetType("System.String")))
        fs.DataTable.Columns.Add(New DataColumn("ANZ_STUETZ", Type.GetType("System.Int32")))
        fs.DataTable.Columns.Add(New DataColumn("BAHN_LAENG", Type.GetType("System.Int32")))
        fs.DataTable.Columns.Add(New DataColumn("KENNZEICHN", Type.GetType("System.String")))
        fs.DataTable.Columns.Add(New DataColumn("DATUM_MELD", Type.GetType("System.String")))
        fs.DataTable.Columns.Add(New DataColumn("DATUM_ABBA", Type.GetType("System.String")))
        fs.DataTable.Columns.Add(New DataColumn("FARBE", Type.GetType("System.String")))
        fs.DataTable.Columns.Add(New DataColumn("STANDORT", Type.GetType("System.String")))
        fs.DataTable.Columns.Add(New DataColumn("BEZIRK", Type.GetType("System.String")))
        fs.DataTable.Columns.Add(New DataColumn("STATUS", Type.GetType("System.String")))



        Dim id = 0

        For Each cli In parseResult
            Dim cl As New Coordinate(cli.lon, cli.lat)


            Dim ffa As IFeature = fs.AddFeature(New Point(cl))
            ffa.DataRow.AcceptChanges()

            ffa.DataRow("LFNDNR") = id
            ffa.DataRow("ART") = cli.type
            ffa.DataRow("OBJNAME") = cli.name
            ffa.DataRow("H_MAX") = cli.height
            ffa.DataRow("H_TAL") = cli.elevation
            ffa.DataRow("H_MAX") = cli.height
            ffa.DataRow("DATUM_MELD") = "1970-01-01 00:00:00"
            ffa.DataRow("DATUM_ABBA") = ""
            ffa.DataRow("STANDORT") = cli.name
            ffa.DataRow("STATUS") = "n"
            id += 1
        Next
        fs.SaveAs("out/aipObstacleConverter_dfs.shp", True)

        ' write feature code file
        Dim file As System.IO.StreamWriter
        file = My.Computer.FileSystem.OpenTextFileWriter("out/featureCodes_dfs.txt", True)


        file.WriteLine("[Appearance]")
        file.WriteLine("FeatureClass=DFS_obstacles*,type=CHIMNEY,310")
        file.WriteLine("FeatureClass=DFS_obstacles*,type=TOWER,311")
        file.WriteLine("FeatureClass=DFS_obstacles*,type=WINDTURBINE,312")
        file.WriteLine("FeatureClass=DFS_obstacles*,type=MAST,313")
        file.WriteLine("FeatureClass=DFS_obstacles*,type=CRANE,314")
        file.WriteLine("[Label]")
        file.WriteLine("FeatureClass=DFS_obstacles*,height")

        file.Close()


    End Sub
End Module
