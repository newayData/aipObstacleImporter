Imports System.IO
Imports System.Text.RegularExpressions
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


        Dim coordPairs_lst As New List(Of String)
        Dim elevation As Double = 0
        Dim height As Double = 0

        For Each cline In textLine
            Dim pattern As String = "[0-8][0-9] [0-5][0-9] [0-9][0-9][NS] [0-3][0-8][0-9] [0-5][0-9] [0-9][0-9][EW] "

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
                    cline = cline.Replace(m.Value, "")

                Else
                    mode = "newItem"


                    cline = cline.Replace(m.Value, "")

                    ' get the name
                    Dim rw = cline.Clone
                    rw = rw.replace(" / ", "\").split(" ")
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
                            If dist < 1 Then pointInGroup = True
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

                Dim state As Short = 0
                Dim patternElev As String = "( [1-9][0-9][0-9][0-9]|[1-9][0-9][0-9]|[1-9][0-9]|[1-9])"
                Dim elementSplitPost = cline.ToString.Replace(" / ", " /").Split(" ")
                For Each item In elementSplitPost
                    If item.Contains("/") = False Then
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
                    End If


                Next


                Select Case mode
                    Case "coords"
                        If coordPairs_lst.Contains(m.Value) = False Then coordPairs_lst.Add(m.Value)
                        cline = cline.Replace(m.Value, "")



                    Case "newItem"

                        coordPairs_lst.Clear()
                        Console.WriteLine("--------------------")
                        Console.WriteLine(name)


                        If coordPairs_lst.Contains(m.Value) = False Then coordPairs_lst.Add(m.Value)

                        cline = cline.Replace(m.Value, "")



                End Select

                Console.WriteLine(m.Value & "  elev: " & elevation & "     height: " & height)
                Console.ReadKey()


                m = r.Match(cline)
            Loop




        Next







        'Dim m As Match = r.Match(rawtext)
        'Dim matchcount As Integer = 0
        'Do While m.Success
        '    Dim targetAddress = m.Index
        '    Dim PreBufferLength As Short = 100
        '    Dim PostBufferLength As Short = 100
        '    If targetAddress < PreBufferLength Then PreBufferLength = targetAddress
        '    If rawtext.Length < targetAddress + PostBufferLength Then PostBufferLength = rawtext - targetAddress

        '    ' text before the found element
        '    Dim elementStringPre = rawtext.Substring(targetAddress - PreBufferLength, PreBufferLength)
        '    Dim spChar As Char() = {vbLf}
        '    Dim elementSplitPre = elementStringPre.Split(spChar).ToList














        '    elementSplitPre.Reverse()

        '    ' text after the found element
        '    Dim elementStringPost = rawtext.Substring(targetAddress, PostBufferLength)
        '    Dim elementSplitPost = elementStringPost.Split(" ").ToList













        ' ========================
        ' get obstacle type
        '' ========================
        'Dim type_lst As New List(Of String)
        '    Dim type As String = ""
        '    Dim counter As Short = -100

        '    Dim ignoreListe As New List(Of String)

        '    Dim matches As New List(Of String)
        '    For Each item In elementSplitPre
        '        matches.Add(item)
        '        ' find the first usefule word
        '        Dim p As String = "\b[A-Za-z]{4,}\b"

        '        ' Instantiate the regular expression object.
        '        Dim s As Regex = New Regex(p, RegexOptions.IgnoreCase)

        '        Dim q As Match = s.Match(item)

        '        ' use the second word found...
        '        If q.Success Then
        '            Dim definition = checkWord(item)
        '            If definition Then

        '                ' A string is returned if the word exists in the dictionary
        '                'Console.WriteLine(CStr(definition))

        '                type_lst.Add(item)
        '                counter = 1
        '            End If

        '            counter += 1
        '            If counter > 3 Then Exit For
        '            ignoreListe.Add(item)

        '        End If
        '    Next

        '    type_lst.Reverse()
        '    For Each t In type_lst
        '        type &= " " & t
        '    Next

        '    ' ========================
        '    ' get obstacle name
        '    ' ========================
        '    ' look until a new line is found
        '    Dim name_lst As New List(Of String)
        '    Dim name As String = ""
        '    For Each item In elementSplitPre
        '        If item <> "" And ignoreListe.Contains(item) = False Then
        '            ' find the first usefule word
        '            Dim p As String = "\b[A-Za-z]{4,}\b"

        '            ' Instantiate the regular expression object.
        '            Dim s As Regex = New Regex(p, RegexOptions.IgnoreCase)

        '            Dim q As Match = s.Match(item)

        '            ' use the second word found...
        '            If q.Success Then

        '                name_lst.Add(item)

        '            End If
        '        End If
        '    Next

        '    Dim wordCnt = 0
        '    For Each item In name_lst
        '        name &= " " & item
        '        wordCnt +=1

        '        If wordCnt = 1 Then Exit For
        '    Next


        '    '' ========================
        '    '' latitude
        '    '' ========================
        '    Dim patternLat As String = "[NS] [0-8][0-9] [0-5][0-9] [0-9][0-9]"

        '    '' Instantiate the regular expression object.
        '    Dim latex As Regex = New Regex(patternLat, RegexOptions.IgnoreCase)
        '    Dim latMatch As Match = latex.Match(elementStringPost)

        '    Dim latitudeString As String = latMatch.Value

        '    'Dim latitude As Double = lat2Double(latitudeString)

        '    '' ========================
        '    '' longitude
        '    '' ========================
        '    Dim patternLon As String = "[EW] [0-3][0-8][0-9] [0-5][0-9] [0-9][0-9]"

        '    '' Instantiate the regular expression object.
        '    Dim lonex As Regex = New Regex(patternLon, RegexOptions.IgnoreCase)
        '    Dim lonMatch As Match = lonex.Match(elementStringPost)

        '    Dim longitudeString As String = lonMatch.Value

        '    'Dim longitude As Double = lon2Double(latitudeString)

        '    'If type = "" Then
        '    '    Console.ForegroundColor = ConsoleColor.Yellow
        '    '    Console.WriteLine("WARN: not found type! " & name)
        '    '    Console.ResetColor()
        '    'End If

        '    '' ========================
        '    '' elevation  / height
        '    '' ========================
        '    Dim elevation As Double = 0
        '    Dim height As Double = 0
        '    'Dim state As Short = 0
        '    'Dim patternElev As String = "([1-9][0-9][0-9][0-9]|[1-9][0-9][0-9])"
        '    'For Each item In elementSplitPost
        '    '    ' Instantiate the regular expression object.
        '    '    Dim elevex As Regex = New Regex(patternElev, RegexOptions.IgnoreCase)
        '    '    Dim elevMatch As Match = elevex.Match(item)

        '    '    If elevMatch.Success Then
        '    '        If state = 0 Then
        '    '            elevation = elevMatch.Value
        '    '            state = 1
        '    '        Else
        '    '            height = elevMatch.Value
        '    '            Exit For
        '    '        End If
        '    '    End If
        '    'Next



        '    'If elevation = 0 Or height = 0 Then
        '    '    Console.ForegroundColor = ConsoleColor.Yellow
        '    '    Console.WriteLine("WARN: elevation or height not found! " & name)
        '    '    Console.ResetColor()
        '    'End If


        '    '' ========================
        '    '' lighted  / marked
        '    '' ========================
        '    Dim lighted As Boolean = False
        '    Dim marked As Boolean = False
        '    'Dim state2 As Short = 0
        '    'For Each item In elementSplitPost
        '    '    If item.Contains("yes") Or item.Contains("no") Then
        '    '        If state2 = 0 Then
        '    '            If item.Contains("yes") Then
        '    '                lighted = True
        '    '            End If
        '    '            state2 = 1
        '    '        Else
        '    '            If item.Contains("yes") Then
        '    '                marked = True
        '    '                Exit For
        '    '            End If
        '    '        End If
        '    '    End If
        '    'Next

        '    Console.WriteLine((name.PadLeft(40)) & "-> " & type.PadLeft(30) & " -> " & latitudeString & " | " & longitudeString & " -> elev: " & elevation.ToString.PadLeft(5) & " -> height: " & height.ToString.PadLeft(5) & " ->  lighted: " & lighted & " -> marked: " & marked)

        '    ' text after the found element

        '    m = m.NextMatch()

        '    '  Console.ReadKey()
        '  Loop



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
        ParsePdfText("testfiles\LO.pdf")
    End Sub

End Module
