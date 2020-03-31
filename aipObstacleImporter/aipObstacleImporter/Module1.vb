Imports Microsoft.SmallBasic.Library
Imports System.Text.RegularExpressions
Imports iTextSharp.text.pdf
Imports iTextSharp.text.pdf.parser

Imports System.Text
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
            newObs.elevation = elevation - height
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
            createCsv()

        Catch ex As Exception
            Console.WriteLine("ERR: " & ex.Message)
        End Try

    End Sub

    ' obsolete function
    Sub createShapefile()

        ' create out folder
        If System.IO.Directory.Exists("\out") = False Then
            System.IO.Directory.CreateDirectory("\out")
        End If

        Dim fs As New FeatureSet(FeatureType.Line)
        fs.DataTable.Columns.Add(New DataColumn("ID", Type.GetType("System.Int32")))

        fs.DataTable.Columns.Add(New DataColumn("TYPE", Type.GetType("System.String")))
        fs.DataTable.Columns.Add(New DataColumn("NAME", Type.GetType("System.String")))
        fs.DataTable.Columns.Add(New DataColumn("HEIGHT", Type.GetType("System.String")))
        fs.DataTable.Columns.Add(New DataColumn("ELEVATION", Type.GetType("System.Int32")))


        Dim id = 0

        For Each cli In parseResult

            Dim cl As New Coordinate(cli.lon, cli.lat)


            Dim ffa As IFeature = fs.AddFeature(New Point(cl))
            ffa.DataRow.AcceptChanges()

            ' cast type
            Dim casetType = "TOWER"
            Dim tags = cli.name & cli.type



            If tags.ToLower.Contains("mast") Or tags.ToLower.Contains("antenna") Then
                casetType = "MAST"
            End If

            If tags.ToLower.Contains("wind") Then
                casetType = "WINDTURBINE"
            End If
            If tags.ToLower.Contains("crane") Then
                casetType = "CRANE"
            End If
            If tags.ToLower.Contains("chimney") Or tags.Contains("plant") Then
                casetType = "CHIMNEY"
            End If



            ffa.DataRow("ID") = id
            ffa.DataRow("TYPE") = casetType
            ffa.DataRow("HEIGHT") = Math.Round((cli.height + cli.elevation) / 10) * 10 + "'"
            ffa.DataRow("ELEVATION") = cli.elevation
            ffa.DataRow("NAME") = cli.name

            id += 1


        Next
        fs.SaveAs("out/obstacle.shp", True)

        ' write feature code file
        Dim file As System.IO.StreamWriter
        file = My.Computer.FileSystem.OpenTextFileWriter("out/featureCodes.txt", False)


        file.WriteLine("[Appearance]")

        file.WriteLine("FeatureClass=obstacle*,type=CHIMNEY,300")
        file.WriteLine("FeatureClass=obstacle*,type=TOWER,311")
        file.WriteLine("FeatureClass=obstacle*,type=WINDTURBINE,312")
        file.WriteLine("FeatureClass=obstacle*,type=MAST,313")
        file.WriteLine("FeatureClass=obstacle*,type=CRANE,314")
        file.WriteLine("[Label]")
        file.WriteLine("FeatureClass=obstacle*,height")

        file.Close()


    End Sub

    Dim outfile = "out\obstacles_ED.csv"
    Sub createCsv()

        Dim ff As New System.IO.StreamWriter(outfile, False)
        writeHeadline(ff)

        If parseResult Is Nothing Then
            Console.WriteLine("ERR: export empty!")
            Return
        End If

        Dim cn As Long = -1
        For Each master In parseResult

            cn += 1

            ' if source is given, replace

            writeLine(ff, cn, 0, master.name, "", master.type, master.lighted, False, "FT", master.elevation, master.height, master.lat, master.lon, False, 0, 0, 0, 0, "", "GERMAN AIP ENR 5.4")
        Next

        ff.Close()

        Console.WriteLine("wrote " & parseResult.Count & " objects")

        If parseResult.Count > 300 Then
            Console.WriteLine("TASKFINISHED")
        Else
            Console.WriteLine("ERR: few objects!")
        End If

    End Sub
    Sub writeHeadline(wrt As System.IO.TextWriter)
        ' headline
        wrt.WriteLine("codeType;txtName;codeLgt;txtDescrMarking;geoLat;geoLong;valElev;valGeoAccuracy;valHgt;codeHgtAccuracy;uomDistVer;valRadius;valRadiusAccuracy;uomRadius;codeGroupId;txtGroupName;locGroupMemberId;locLinkedToGroupMemberId;codeLinkType;datetimeValidWef;datetimeValidTil;txtRmk;source;defaultHeightFlag;codeMarking".Replace(";", seperator))
    End Sub
    Dim headerline() As String = Nothing
    Dim seperator As String = ","
    Structure csvStruct
        Dim codeGroup As String
        Dim groupInternalId As Long
        Dim groupDescription As String
        Dim name As String
        Dim type As String
        Dim lighted As Boolean
        Dim markingDescription As String
        Dim heightUnit As String
        Dim heightValue As Long
        Dim elevationValue As Long
        Dim latitude As Double
        Dim longitude As Double
        Dim defaultHeightFlag As Boolean
        Dim verticalPrecision As Double
        Dim lateralPrecision As Double
        Dim obstacleRadius As Double
        Dim linkedToGroupInternalId As Long
        Dim linkType As String
        Dim validUntil As Date
        Dim effectiveFrom As Date
        Dim origin As String
        Dim _used As Boolean
    End Structure
    Sub writeLine(writer As System.IO.StreamWriter, codegroup As String, internalId As String, name As String, groupdescr As String, type As String, lighted As Boolean, marking As Boolean, heightUnit As String, elevationValue As Long, heightValue As Long, latitude As Double, longitude As Double, defaultHeightFlag As Boolean, lateralPrecision As Double, verticalPrecision As Double, obstacleRadius As Double, linkedToGroupInternalId As String, linkType As String, source As String)
        If groupdescr Is Nothing Then groupdescr = ""
        If codegroup Is Nothing Then codegroup = ""
        If internalId Is Nothing Then internalId = ""
        If name Is Nothing Then name = ""
        If groupdescr Is Nothing Then groupdescr = ""
        If type Is Nothing Then type = ""
        If heightUnit Is Nothing Then heightUnit = ""
        If linkedToGroupInternalId Is Nothing Then linkedToGroupInternalId = ""
        If linkType Is Nothing Then linkedToGroupInternalId = ""

        If name = "" Then name = "Cable"
        If type = "" Then type = "building"

        If type.ToLower.Contains("wind") Then type = "WINDTURBINE"

        If type.ToLower.Contains("antenna") Then type = "ANTENNA"

        If type.ToLower.Contains("mast") Then type = "MAST"
        If type.ToLower.Contains("plant") Then type = "BUILDING"

        If type <> "WINDTURBINE" And type <> "ANTENNA" And type <> "MAST" And type <> "BUILDING" Then
            type = "TOWER"
        End If



        linkedToGroupInternalId = linkedToGroupInternalId.Replace(seperator, " ")
        linkType = linkType.Replace(seperator, " ")



        Dim doublications = codegroup & ";" & internalId

        Dim lineStr = "{codeType}{sepr}{txtName}{sepr}{codeLgt}{sepr}{txtDescrMarking}{sepr}{geoLat}{sepr}{geoLong}{sepr}{valElev}{sepr}{valElevAccuracy}{sepr}{valHgt}{sepr}{codeHgtAccuracy}{sepr}{uomDistVer}{sepr}{valRadius}{sepr}{valRadiusAccuracy}{sepr}{uomRadius}{sepr}{codeGroupId}{sepr}{txtGroupName}{sepr}{locGroupMemberId}{sepr}{locLinkedToGroupMemberId}{sepr}{codeLinkType}{sepr}{datetimeValidWef}{sepr}{datetimeValidTil}{sepr}{txtRmk}{sepr}{source}{sepr}{defaultHeightFlag}{sepr}{codeMarking}"

        lineStr = lineStr.Replace("{sepr}", seperator)

        lineStr = lineStr.Replace("{codeType}", type.Replace(seperator, " "))
        lineStr = lineStr.Replace("{txtName}", name.Replace(seperator, " "))


        Dim defHeight = "N"

        If defaultHeightFlag = True Then
            defHeight = "Y"
        Else
            If heightValue = 0 Then
                defHeight = "Y"

                heightUnit = "m"
            End If
        End If


        Dim codeLGT = "N"
        If lighted Then codeLGT = "Y"
        lineStr = lineStr.Replace("{codeLgt}", codeLGT.Replace(seperator, " "))
        lineStr = lineStr.Replace("{txtDescrMarking}", "".Replace(seperator, " "))
        lineStr = lineStr.Replace("{geoLat}", latitude.ToString.Replace(",", ".").Replace(seperator, " "))
        lineStr = lineStr.Replace("{geoLong}", longitude.ToString.Replace(",", ".").Replace(seperator, " "))
        lineStr = lineStr.Replace("{valElev}", elevationValue.ToString.Replace(seperator, " "))
        lineStr = lineStr.Replace("{valElevAccuracy}", "0".Replace(seperator, " "))
        lineStr = lineStr.Replace("{valHgt}", heightValue.ToString.Replace(seperator, " "))
        lineStr = lineStr.Replace("{codeHgtAccuracy}", "1".Replace(seperator, " "))
        lineStr = lineStr.Replace("{uomDistVer}", heightUnit.Replace(seperator, " "))
        lineStr = lineStr.Replace("{valRadius}", 0)
        lineStr = lineStr.Replace("{valRadiusAccuracy}", lateralPrecision.ToString.Replace(",", ".").Replace(seperator, " "))
        lineStr = lineStr.Replace("{uomRadius}", heightUnit.Replace(seperator, " "))
        lineStr = lineStr.Replace("{codeGroupId}", codegroup.Replace(seperator, " "))
        lineStr = lineStr.Replace("{txtGroupName}", groupdescr.Replace(seperator, " "))
        lineStr = lineStr.Replace("{locGroupMemberId}", internalId.ToString.Replace(seperator, " "))
        lineStr = lineStr.Replace("{locLinkedToGroupMemberId}", linkedToGroupInternalId.ToString.Replace(seperator, " "))
        lineStr = lineStr.Replace("{codeLinkType}", linkType.Replace(seperator, " "))
        lineStr = lineStr.Replace("{datetimeValidWef}", "")
        lineStr = lineStr.Replace("{datetimeValidTil}", "")
        lineStr = lineStr.Replace("{txtRmk}", "")
        lineStr = lineStr.Replace("{source}", source.Replace(seperator, " "))

        lineStr = lineStr.Replace("{defaultHeightFlag}", defHeight.ToString.Replace(",", ".").Replace(seperator, " "))
        lineStr = lineStr.Replace("{codeMarking}", "N".Replace(seperator, " "))

        ' final syntax check
        If lineStr.Contains("{") = False And lineStr.Contains("}") = False Then

            writer.WriteLine(lineStr)
        End If
    End Sub
End Module
