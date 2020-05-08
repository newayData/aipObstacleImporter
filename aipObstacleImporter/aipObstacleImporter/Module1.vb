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
        Dim lightcode As String
        Dim type As String
        Dim name As String
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

        ' sweden special garbage remove
        Dim outText As New List(Of String)
        For i As Short = 0 To textLine.Length - 1
            Dim str = textLine(i)

            ' check if coordinates are in the line

            If Regex.Match(str, "[0-5][0-9] [0-5][0-9] [0-5][0-9]N 0[0-5][0-9] [0-5][0-9] [0-5][0-9]E").Success Then

                If str.Contains(" * ") Then
                    str = str.Replace(" * ", " ")
                End If

                outText.Add(str)
            End If
        Next
        textLine = outText.ToArray

        Dim obstacleGroup As New groupStruct
        obstacleGroup.element = New List(Of obstStruct)

        Dim coordPairs_lst As New List(Of String)
        Dim elevation As Double = 0
        Dim height As Double = 0
        Dim clineCnt As Short = 0

        Dim name As String = ""
        Dim type As String = ""
        For Each cline In textLine
            Dim pattern As String = "[0-5][0-9] [0-5][0-9] [0-5][0-9]N 0[0-5][0-9] [0-5][0-9] [0-5][0-9]E"

            Try

                ' Instantiate the regular expression object.
                Dim r As Regex = New Regex(pattern, RegexOptions.IgnoreCase)
                Dim m As Match = r.Match(cline)

                Dim lighttype As String = ""


                m = r.Match(cline)

                lighttype = Regex.Match(cline, "F[A-Z ]{0,4}R").Value
                Dim ob As New obstStruct
                ob.pos = coord2double(m.Value)

                ' make anchor hook instead of position
                If lighttype = "" Then lighttype = "?"
                cline = cline.Replace(m.Value, "$").Replace(lighttype, "")

                Dim rightClip As String = cline.Split("$")(1)
                Dim leftClip As String = cline.Split("$")(0)


                If leftClip <> "" Then name = leftClip ' to group the name as not every line has designators

                Try
                    Dim sps = rightClip.Split({" "}, StringSplitOptions.RemoveEmptyEntries)

                    height = sps(0)
                    elevation = sps(1)
                    type = name
                Catch ex As Exception
                End Try

                ob.elevation = elevation - height
                ob.height = height
                ob.lightcode = lighttype
                ob.rawpos = m.Value
                ob.type = type
                ob.name = name

                obstacleGroup.element.Add(ob)

                Console.Write(".")
            Catch ex As Exception
                Console.WriteLine("ERR: " & ex.Message)
            End Try
        Next

        resGroups.Add(obstacleGroup)

        Return Nothing

    End Function




    <Serializable()> Structure DoublePointStruct
        Dim x As Double
        Dim y As Double
        Dim rmk As String
    End Structure



    Dim decimalSeparator As String = Globalization.CultureInfo.CurrentCulture.NumberFormat.NumberDecimalSeparator
    Function coord2double(str As String) As DoublePointStruct

        Dim retC As New DoublePointStruct


        Try
            Dim sp = str.Split(" ")

            Dim degLat As Short = str.Substring(0, 2)
            Dim minLat As Short = str.Substring(3, 2)
            Dim secLat As Double = str.Substring(6, 2).Replace(".", decimalSeparator)

            Dim degLon As Short = str.Substring(10, 3)
            Dim minLon As Short = str.Substring(14, 2)
            Dim secLon As Double = str.Substring(17, 2).Replace(".", decimalSeparator)


            retC.y = degLat + minLat / 60 + secLat / 3600
            retC.x = degLon + minLon / 60 + secLon / 3600
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

 
        createCsv()
    End Sub


    Dim outFile = "out/obstacles_EK.csv"
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



                writeLine(ff, cn, ssp, master.name, "", master.type, master.lightcode <> "?", False, "FT", master.elevation, master.height, master.pos.y, master.pos.x, False, 0, 0, 0, ssp + 1, linktype, "AIP Denmark ENR 5.4")

            Next
        Next

        ff.Close()

        Console.WriteLine("wrote " & ssp & " objects")



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
