Imports System.IO
Imports System.Text.RegularExpressions
Imports DotSpatial.Data
Imports DotSpatial.Topology
Imports iTextSharp.text.pdf
Imports iTextSharp.text.pdf.parser
Imports Microsoft.SmallBasic.Library

Module Module1

    Dim Version As String = "1.0"

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

    Structure obstStruct
        Dim pos As DoublePointStruct
        Dim rawpos As String
        Dim elevation As Long
        Dim height As Long
    End Structure
    Structure groupStruct
        Dim name As String
        Dim type As String
        Dim element As List(Of obstStruct)
    End Structure

    Dim resGroups As New List(Of groupStruct)


    Function processText(rawtext As String) As Task

        ' to differentiate between name and type, load the ignore list: nameignorelist.txt
        Dim ignore_lst As New List(Of String)

        Try
            Dim reader As StreamReader = My.Computer.FileSystem.OpenTextFileReader("nameignorelist.txt")
            Dim a As String

            Do

                a = reader.ReadLine

                If ignore_lst.Contains(a) = False And a <> "" Then ignore_lst.Add(a)
            Loop Until a Is Nothing

            reader.Close()
        Catch

        End Try

        Dim textLine = rawtext.Split(vbLf)


        Dim obstacleGroup As New groupStruct
        obstacleGroup.element = New List(Of obstStruct)

        Dim coordPairs_lst As New List(Of String)
        Dim elevation As Double = 0
        Dim height As Double = 0

        Dim oldStat As String = "newItem"
        Dim clineCnt As Short = 0
        Dim nextline As String = ""
        For Each cline In textLine
            Dim pattern As String = "[0-8][0-9] [0-5][0-9] [0-9][0-9][NS] [0-3][0-8][0-9] [0-5][0-9] [0-9][0-9][EW] "

            Try
                nextline = ""
                For i As Short = clineCnt + 1 To clineCnt + 1
                    nextline &= " " & textLine(i)
                Next
            Catch ex As Exception
            End Try

            clineCnt += 1

            ' Instantiate the regular expression object.
            Dim r As Regex = New Regex(pattern, RegexOptions.IgnoreCase)
            Dim m As Match = r.Match(cline)

            Dim mode As String = "newItem"

            Dim name As String = ""
            Dim type As String = ""

            Do While m.Success

                m = r.Match(cline)

                If m.Index = 0 Then     ' this means additional coords
                    mode = "coords"
                    coordPairs_lst.Add(m.Value)
                    cline = cline.Replace(m.Value, "### ")

                Else
                    mode = "newItem"


                    cline = cline.Replace(m.Value, "### ")

                    ' get the name
                    ' remove all after the coordinates
                    Dim rq As String = ""
                    For Each item In cline
                        If item = "#" Then Exit For
                        rq &= item
                    Next

                    Dim rw = rq.Split(" ")
                    Dim rwClean As New List(Of String)
                    For Each item In rw
                        If item.ToString.Contains("\") = False Then
                            rwClean.Add(item)
                        End If
                    Next


                    ' check distance to last point
                    ' center of prev points
                    Dim pointInGroup As Boolean = False
                    For Each p In coordPairs_lst
                        Dim nepp = coord2double(p)
                        Dim newpoint = coord2double(m.Value)

                        Dim dist = GetGreatCircleDistance_ConstEarthRadiusInNm(nepp, newpoint)
                        If dist <> 0 Then
                            If dist < 2 Then pointInGroup = True
                        End If
                    Next

                    If pointInGroup Then
                        mode = "coords"
                        For i As Short = 0 To rwClean.Count - 1
                            type &= " " & rwClean(i)
                        Next

                    Else
                        mode = "newItem"
                        For i As Short = 0 To rwClean.Count - 1
                            name &= " " & rwClean(i)
                        Next
                    End If


                End If

                ' get elevation

                ' ========================
                ' elevation  / height
                ' ========================

                cline = cline.Replace("*", "")



                Dim elementSplitPost = (cline).ToString.Split(" ")
                Dim nextLineRead As Boolean = False

againWithNextLine:
                Dim state As Short = 0
                height = 0
                Dim heightSpotted As Boolean = False

                Dim cnt As Short = 0

                ' remove all entries before the coordinates
                Dim coordsFound As Boolean = False
                For i As Short = 0 To elementSplitPost.Count - 1
                    If elementSplitPost(i).Contains("###") Then coordsFound = True

                    If Not coordsFound Then
                        elementSplitPost(i) = "xx"
                    End If
                Next

                ' if read nextline: also remove coordinates and values before, to not confuse

                If nextLineRead Then

                    Try
                        Dim qr = r.Match(nextline)

                        If qr.Value <> "" Then
                            nextline = cline.Replace(qr.Value, "### ")

                            ' find chars
                            Dim removeChars As String = ""
                            For chas As Short = 0 To nextline.Length - 1
                                If nextline(chas) = "#" Then Exit For
                                removeChars &= nextline(chas)
                            Next
                            nextline = nextline.Replace(removeChars, "")

                        End If

                        elementSplitPost = (cline & " " & nextline).Split(" ")



                    Catch ex As Exception

                    End Try


                End If




                For Each item In elementSplitPost

                    ' ignore the first values
                    Try


                        If item.Contains("/") = False And elementSplitPost(cnt + 1).Contains("/") Then
                            ' Instantiate the regular expression object.
                            Try
                                Dim s As Long = item

                                If state = 0 Then
                                    elevation = item
                                    state = 1


                                Else


                                    If item <> elevation Then
                                        height = item
                                        heightSpotted = True
                                        Exit For
                                    End If




                                End If

                            Catch ex As Exception
                                If state = 1 Then height = -1
                            End Try


                        End If

                        cnt += 1
                    Catch ex As Exception
                        height = -1
                    End Try
                Next

                ' if no height found: add nextline
                If heightSpotted = False And nextLineRead = False Then

                    Console.WriteLine("height Not identified, spot Next line.. >> " & name)
                    nextLineRead = True
                    GoTo againWithNextLine
                End If


                If (oldStat = "coords" And mode = "newItem") Or (mode = "newItem" And oldStat = "newItem") Then
                    resGroups.Add(obstacleGroup)


                    obstacleGroup = New groupStruct
                    obstacleGroup.element = New List(Of obstStruct)
                End If


                Select Case mode
                    Case "coords"
                        If coordPairs_lst.Contains(m.Value) = False Then coordPairs_lst.Add(m.Value)
                        cline = cline.Replace(m.Value, "")

                        Dim ob As New obstStruct
                        ob.elevation = elevation
                        ob.height = height
                        ob.rawpos = m.Value
                        ob.pos = coord2double(m.Value)
                        obstacleGroup.element.Add(ob)


                    Case "newItem"

                        ' add pre one
                        Dim ob As New obstStruct
                        ob.elevation = elevation
                        ob.height = height
                        ob.pos = coord2double(m.Value)
                        ob.rawpos = m.Value
                        obstacleGroup.element.Add(ob)
                        obstacleGroup.name = name
                        obstacleGroup.type = type

                        coordPairs_lst.Clear()

                        If coordPairs_lst.Contains(m.Value) = False Then coordPairs_lst.Add(m.Value)

                        cline = cline.Replace(m.Value, "")

                End Select

                oldStat = mode
                m = r.Match(cline)

                Console.Write(".")
            Loop
        Next

        For o As Short = 0 To resGroups.Count - 1
            Try


                ' highest values in height
                Dim maxHeight As Long = 0
                For i As Short = 0 To resGroups(o).element.Count - 1
                    If resGroups(o).element(i).height > maxHeight Then maxHeight = resGroups(o).element(i).height
                Next

                ' set max height
                For i As Short = 0 To resGroups(o).element.Count - 1

                    If resGroups(o).element(i).height = -1 Then

                        Console.WriteLine("!added max height, as no unique entry in source document! >> " & resGroups(o).name)
                        Dim sd = resGroups(o).element(i)
                        sd.height = maxHeight
                        resGroups(o).element(i) = sd
                    End If
                Next

            Catch ex As Exception

            End Try
        Next

        For Each item In resGroups
            Console.WriteLine("----------------------")
            Console.WriteLine(item.name & " " & item.type)

            For Each els In item.element
                Console.WriteLine("points: " & els.rawpos & "  elev: " & els.elevation & "  height:" & els.height)
            Next

            'Console.ReadKey()
        Next




        Return Nothing

    End Function

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

    Structure VectorStruct
        Dim x As Double
        Dim y As Double
        Dim z As Double
    End Structure
    <Serializable()> Structure DoublePointStruct
        Dim x As Double
        Dim y As Double
        Dim rmk As String
    End Structure

    ' Coordinate Transformations
    Dim EarthRadius As Double = 6378.137 / 1.852 ' in nautical Miles
    Function GetGreatCircleDistance_ConstEarthRadiusInNm(ByVal Position1 As DoublePointStruct, ByVal Position2 As DoublePointStruct) As Double
        Dim cos_phiA As Double = Math.Cos(Position1.y * Math.Pi / 180)
        Dim cos_phiB As Double = Math.Cos(Position2.y * Math.Pi / 180)
        Dim sin_phiA As Double = Math.Sin(Position1.y * Math.Pi / 180)
        Dim sin_phiB As Double = Math.Sin(Position2.y * Math.Pi / 180)
        Dim cos_lambdaBLamdaA As Double = Math.Cos((Position2.x - Position1.x) * Math.Pi / 180)

        Dim dist As Double = EarthRadius * Math.ArcCos(sin_phiA * sin_phiB + cos_phiA * cos_phiB * cos_lambdaBLamdaA)

        Return dist

        'Notes from 22.3.2012
        Dim V1 As VectorStruct
        V1.x = EarthRadius * (Math.Cos(Position1.y * Math.Pi / 180) * Math.Cos(Position1.x * Math.Pi / 180))
        V1.y = EarthRadius * (Math.Cos(Position1.y * Math.Pi / 180) * Math.Sin(Position1.x * Math.Pi / 180))
        V1.z = EarthRadius * (Math.Sin(Position1.y * Math.Pi / 180))

        Dim V2 As VectorStruct
        V2.x = EarthRadius * (Math.Cos(Position2.y * Math.Pi / 180) * Math.Cos(Position2.x * Math.Pi / 180))
        V2.y = EarthRadius * (Math.Cos(Position2.y * Math.Pi / 180) * Math.Sin(Position2.x * Math.Pi / 180))
        V2.z = EarthRadius * (Math.Sin(Position2.y * Math.Pi / 180))

        Dim AngleBetw As Double = Math.ArcCos((V1.x * V2.x + V1.y * V2.y + V1.z * V2.z) / (Math.SquareRoot(V1.x ^ 2 + V1.y ^ 2 + V1.z ^ 2) * Math.SquareRoot(V2.x ^ 2 + V2.y ^ 2 + V2.z ^ 2)))

        ' Return Distance in nautical Miles

        Dim vl = AngleBetw * EarthRadius

        Dim diff As Double = dist - vl

        Return AngleBetw * EarthRadius '* 1.0015
    End Function

    Function coord2double(str As String) As DoublePointStruct

        Dim retC As New DoublePointStruct


        Try
            Dim sp = str.Split(" ")
            retC.y = sp(0) + sp(1) / 60 + sp(2).Replace("N", "").Replace("S", "") / 3600
            retC.x = sp(3) + sp(4) / 60 + sp(5).Replace("E", "").Replace("W", "") / 3600
            Return retC
        Catch ex As Exception
            Console.WriteLine("ERR: cant recognize longitude! " & str)
        End Try
        Return Nothing
    End Function



    Sub Main()
        writeHeader()
        For Each file In Directory.GetFiles("aipDocs")

            If file.EndsWith(".pdf") Then
                ParsePdfText(file)
            End If

        Next

        createShapefile()
        createCsv()
    End Sub

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

        For Each cli In resGroups
            For Each el In cli.element
                Dim cl As New Coordinate(el.pos.x, el.pos.y)


                Dim ffa As IFeature = fs.AddFeature(New Point(cl))
                ffa.DataRow.AcceptChanges()

                ' cast type
                Dim casetType = "TOWER"
                Dim tags = cli.name & cli.type

                If cli.type = "" Then

                    If tags.ToLower.Contains("mast") Then
                        casetType = "MAST"
                    End If

                    If tags.ToLower.Contains("wind") Then
                        casetType = "WINDTURBINE"
                    End If
                    If tags.ToLower.Contains("crane") Then
                        casetType = "CRANE"
                    End If
                    If tags.ToLower.Contains("chimney") Then
                        casetType = "CHIMNEY"
                    End If
                End If

                Dim hVal As String = (Math.Round((el.height) / 10) * 10) + " AGL"
                If hVal = "0 AGL" Then hVal = ""

                ffa.DataRow("ID") = id
                ffa.DataRow("TYPE") = casetType
                ffa.DataRow("HEIGHT") = hVal
                ffa.DataRow("ELEVATION") = el.elevation
                ffa.DataRow("NAME") = cli.name

                id += 1
            Next

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

    Dim outFile = "out/obstacles_LO.csv"
    Sub createCsv()


        If System.IO.Directory.Exists("out") = False Then System.IO.Directory.CreateDirectory("out")

        Dim ff As New System.IO.StreamWriter(outFile, False)
        writeHeadline(ff)

        If resGroups Is Nothing Then
            Console.WriteLine("ERR: export empty!")
            Return
        End If

        Dim cn As Long = -1



        Dim ssp As Long = -1
        For Each item In resGroups
            ssp = 0
            For Each master In item.element

                ssp += 1
                cn += 1

                ' if source is given, replace
                Dim linktype As String = "GROUP"

                If item.name.ToLower.Contains("seilbahn") Then
                    linktype = "CABLE"

                End If

                If item.name.Contains("Hausalm/Plöckenpass - Kleiner Pal Seilbahn / Cableway") Then
                    Dim kkk = 3
                End If

                writeLine(ff, cn, ssp, item.name, "", item.type, False, False, "M", master.elevation, master.height, master.pos.y, master.pos.x, False, 0, 0, 0, ssp + 1, linktype, "AUSTRIA AIP ENR 5.4")

            Next
        Next

        ff.Close()

        Console.WriteLine("wrote " & resGroups.Count & " objects")

        If resGroups.Count > 300 Then
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


        If name.ToLower.Contains("seilbahn") Or name.ToLower.Contains("material") Or name.ToLower.Contains("leitung") Then
            linkType = "CABLE"
            type = "BUILDING"
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
