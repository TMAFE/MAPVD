Imports System
Imports System.IO
Imports System.Text
Imports System.Drawing
Imports System.Windows.Forms
Imports System.Collections.Generic

Public Class Form1
    Private _wavList As List(Of WavInfo)

    Private _data As Byte()
    Private _frames As List(Of FrameInfo)
    Private _sections As List(Of Integer)
    Private _actPath As String

    Private Shared ReadOnly PLACEABLE_KEY As Byte() = BitConverter.GetBytes(&H9AC6CDD7UI)

    Private Class FrameInfo
        Public Index As Integer
        Public Offset As Integer
        Public Size As Integer

        Public Overrides Function ToString() As String
            Return String.Format("Frame {0} @ 0x{1:X8} ({2} bytes)", Index, Offset, Size)
        End Function
    End Class

    Private Class WavInfo
        Public Index As Integer
        Public Offset As Integer
        Public Length As Integer
        Public SampleRate As Integer
        Public Channels As Integer
        Public BitsPerSample As Integer
        Public DurationSeconds As Double

        Public Overrides Function ToString() As String
            Dim ms As Integer = CInt(DurationSeconds * 1000.0R)
            Return String.Format("#{0} @0x{1:X6}  {2} ms  {3} Hz  {4} ch  {5} bit",
                                  Index, Offset, ms, SampleRate, Channels, BitsPerSample)
        End Function
    End Class
    Private Sub btnOpen_Click(sender As Object, e As EventArgs) Handles btnOpen.Click
        If OpenFileDialog1.ShowDialog(Me) <> DialogResult.OK Then
            Return
        End If

        Try
            LoadActFile(OpenFileDialog1.FileName)
        Catch ex As Exception
            MessageBox.Show(Me,
                            "Error loading ACT file:" & Environment.NewLine & ex.Message,
                            "Error",
                            MessageBoxButtons.OK,
                            MessageBoxIcon.[Error])
        End Try
    End Sub

    Private Sub ComboBox1_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ComboBox1.SelectedIndexChanged
        If _frames Is Nothing OrElse _frames.Count = 0 Then
            Return
        End If

        Dim idx As Integer = ComboBox1.SelectedIndex
        If idx < 0 OrElse idx >= _frames.Count Then
            Return
        End If

        Dim fi As FrameInfo = _frames(idx)
        ShowFrame(fi)
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        If _wavList Is Nothing OrElse _wavList.Count = 0 Then
            MessageBox.Show(Me, "No sounds found in this ACT file.", "Play sound",
                            MessageBoxButtons.OK, MessageBoxIcon.Information)
            Return
        End If

        Dim idx As Integer = ComboBox2.SelectedIndex
        If idx < 0 OrElse idx >= _wavList.Count Then
            MessageBox.Show(Me, "Please select a sound first.", "Play sound",
                            MessageBoxButtons.OK, MessageBoxIcon.Information)
            Return
        End If

        Try
            PlayWav(_wavList(idx))
        Catch ex As Exception
            MessageBox.Show(Me, "Error playing sound:" & Environment.NewLine & ex.Message,
                            "Audio Error", MessageBoxButtons.OK, MessageBoxIcon.[Error])
        End Try
    End Sub

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        ' nothing yet lol I dont need this quite yet
    End Sub
=
    Private Sub LoadActFile(path As String)
        _actPath = path
        _data = File.ReadAllBytes(path)
        _frames = New List(Of FrameInfo)()
        _sections = New List(Of Integer)()
        _wavList = New List(Of WavInfo)()

        ComboBox1.Items.Clear()
        ComboBox2.Items.Clear()
        PictureBox1.Image = Nothing
        TextBox1.Clear()
        TextBox2.Clear()
        TextBox3.Clear()
        TextBox4.Clear()
        TextBox5.Clear()


        ParseHeader()
        ParseSections()
        ParseFrames()
        ParseWavs()
        ExtractFramesToFolder()
        ExtractWavsToFolder()
        ParseDescriptionText()

        TextBox4.Text = _frames.Count.ToString()

        If _frames.Count > 0 Then
            ComboBox1.SelectedIndex = 0
        End If

        If _wavList.Count > 0 Then
            ComboBox2.SelectedIndex = 0
        End If

        Me.Text = "Microsoft Actor Properties Viewer + Decompiler - " & System.IO.Path.GetFileName(path)

    End Sub

    Private Sub ParseHeader()
        If _data Is Nothing OrElse _data.Length < &H20 Then
            Throw New InvalidDataException("File too small to be an ACT.")
        End If

        Dim sig0 As Byte = _data(0)
        Dim sig1 As Byte = _data(1)
        If sig0 <> AscW("L"c) OrElse sig1 <> AscW("P"c) Then
            Throw New InvalidDataException("Not an ACT file as it the missing 'LP' signature.")
        End If

        Dim version As UShort = ReadUInt16(_data, 2)
        Dim animCount As UShort = ReadUInt16(_data, &HA)   ' animation count (confirmed working with V2, V1 counts should* be accurate [maybe? idk lol]

        Dim nameStart As Integer = &H12
        Dim nameEnd As Integer = nameStart
        While nameEnd < _data.Length AndAlso _data(nameEnd) <> 0
            nameEnd += 1
        End While
        Dim name As String = Encoding.ASCII.GetString(_data, nameStart, nameEnd - nameStart)

        TextBox2.Text = name
        TextBox3.Text = String.Format("0x{0:X4}", CInt(version))
        TextBox5.Text = animCount.ToString()
    End Sub
    Private Sub ParseSections()
        _sections.Clear()

        If _data Is Nothing OrElse _data.Length < 10 Then
            Return
        End If

        Dim sectionCount As Integer = ReadUInt16(_data, 8)
        Dim baseOffset As Integer = &H40

        If sectionCount < 0 Then sectionCount = 0
        If sectionCount > 256 Then sectionCount = 256

        For i As Integer = 0 To sectionCount - 1
            Dim entryPos As Integer = baseOffset + 4 * i

            ' If the table itself runs off the end of the file, it will crash so DONT
            If entryPos + 4 > _data.Length Then
                Exit For
            End If

            Dim offU As UInteger = ReadUInt32(_data, entryPos)

            If offU = 0UI OrElse offU >= CUInt(_data.Length) Then
                Continue For
            End If

            Dim off As Integer = CInt(offU)
            _sections.Add(off)
        Next
    End Sub
    Private Sub ParseFrames()
        ComboBox1.Items.Clear()
        _frames.Clear()
        PictureBox1.Image = Nothing

        If _data Is Nothing Then
            Return
        End If

        Dim pos As Integer = 0
        Dim idx As Integer = 0
        Dim len As Integer = _data.Length

        While pos <= len - PLACEABLE_KEY.Length
            Dim found As Integer = IndexOfBytes(_data, PLACEABLE_KEY, pos)
            If found = -1 Then
                Exit While
            End If

            If found + 22 + 10 > len Then
                Exit While
            End If

            Dim wmfHeaderOffset As Integer = found + 22
            Dim fileSizeWords As UInteger = ReadUInt32(_data, wmfHeaderOffset + 6)
            Dim totalBytesLong As Long = CLng(fileSizeWords) * 2L + 22L

            ' this tells it not to go nuts with a big file size, had to include this because I know someone is probably crazy enough to try
            If totalBytesLong <= 0L OrElse totalBytesLong > CLng(len - found) Then
                pos = found + 4
                Continue While
            End If

            Dim totalBytes As Integer = CInt(totalBytesLong)

            Dim fi As New FrameInfo() With {
                .Index = idx,
                .Offset = found,
                .Size = totalBytes
            }

            _frames.Add(fi)
            ComboBox1.Items.Add(fi.ToString())

            idx += 1
            pos = found + totalBytes
        End While
    End Sub

    Private Sub ShowFrame(fi As FrameInfo)
        If _data Is Nothing Then
            Return
        End If

        Try
            Using ms As New MemoryStream(_data, fi.Offset, fi.Size)
                Dim img As Image = Image.FromStream(ms)
                Dim oldImg As Image = PictureBox1.Image
                PictureBox1.Image = CType(img.Clone(), Image)
                If oldImg IsNot Nothing Then oldImg.Dispose()
            End Using
        Catch ex As Exception
            MessageBox.Show(Me,
                            "Error rendering WMF frame:" & Environment.NewLine & ex.Message,
                            "WMF Error",
                            MessageBoxButtons.OK,
                            MessageBoxIcon.Warning)
        End Try
    End Sub

    Private Sub ExtractFramesToFolder()
        If _frames Is Nothing OrElse _frames.Count = 0 OrElse _data Is Nothing Then
            Return
        End If

        Dim baseDir As String = Path.GetDirectoryName(_actPath)
        Dim folderName As String = Path.GetFileNameWithoutExtension(_actPath) & "_frames"
        Dim outDir As String = Path.Combine(baseDir, folderName)

        Directory.CreateDirectory(outDir)

        For Each fi As FrameInfo In _frames
            Dim outPath As String = Path.Combine(outDir,
                                                 String.Format("frame_{0:000}.wmf", fi.Index))
            Dim buf As Byte() = New Byte(fi.Size - 1) {}
            Buffer.BlockCopy(_data, fi.Offset, buf, 0, fi.Size)
            File.WriteAllBytes(outPath, buf)
        Next
    End Sub
    Private Sub ParseDescriptionText()
        TextBox1.Clear()

        If _data Is Nothing OrElse _sections Is Nothing Then
            Return
        End If

        ' Section index 6 (0-based) for Office 97 ACTs
        If _sections.Count <= 6 Then
            Return
        End If

        Dim start As Integer = _sections(6)
        If start <= 0 OrElse start >= _data.Length Then
            Return
        End If

        Dim length As Integer = _data.Length - start
        Dim raw As Byte() = New Byte(length - 1) {}
        Array.Copy(_data, start, raw, 0, length)

        Dim text As String
        Try
            text = Encoding.Unicode.GetString(raw)
        Catch ex As Exception
            text = ""
        End Try

        text = text.Replace(ChrW(0), String.Empty)
        TextBox1.Text = text.Trim()
    End Sub
    Private Sub ParseWavs()
        _wavList = New List(Of WavInfo)()
        ComboBox2.Items.Clear()

        If _data Is Nothing OrElse _data.Length < 12 Then
            Return
        End If

        Dim riff() As Byte = Encoding.ASCII.GetBytes("RIFF")
        Dim i As Integer = 0
        Dim wavIndex As Integer = 0

        While i <= _data.Length - 12

            If _data(i) = riff(0) AndAlso
               _data(i + 1) = riff(1) AndAlso
               _data(i + 2) = riff(2) AndAlso
               _data(i + 3) = riff(3) Then


                If i + 12 > _data.Length Then Exit While
                If _data(i + 8) <> AscW("W"c) OrElse
                   _data(i + 9) <> AscW("A"c) OrElse
                   _data(i + 10) <> AscW("V"c) OrElse
                   _data(i + 11) <> AscW("E"c) Then
                    i += 1
                    Continue While
                End If


                Dim chunkSize As UInteger = BitConverter.ToUInt32(_data, i + 4)

                Dim remaining As Long = CLng(_data.Length) - CLng(i) - 8L
                If chunkSize = 0UI OrElse CLng(chunkSize) > remaining Then
                    i += 1
                    Continue While
                End If

                Dim fileEnd As Integer = i + 8 + CInt(chunkSize)

                Dim pos As Integer = i + 12
                Dim dataBytes As Integer = 0

                Dim sampleRate As Integer = 0
                Dim channels As Integer = 0
                Dim bitsPerSample As Integer = 0
                Dim avgBytesPerSec As Integer = 0

                While pos + 8 <= fileEnd
                    Dim id0 As Byte = _data(pos)
                    Dim id1 As Byte = _data(pos + 1)
                    Dim id2 As Byte = _data(pos + 2)
                    Dim id3 As Byte = _data(pos + 3)
                    Dim subSize As UInteger = BitConverter.ToUInt32(_data, pos + 4)

                    If CLng(pos) + 8L + CLng(subSize) > CLng(fileEnd) Then
                        Exit While
                    End If

                    If id0 = AscW("f"c) AndAlso id1 = AscW("m"c) AndAlso
                       id2 = AscW("t"c) AndAlso id3 = AscW(" "c) Then

                        Dim fmtPos As Integer = pos + 8
                        Dim fmtSize As Integer = CInt(subSize)

                        If fmtSize >= 16 AndAlso fmtPos + fmtSize <= _data.Length Then
                            channels = BitConverter.ToInt16(_data, fmtPos + 2)
                            sampleRate = BitConverter.ToInt32(_data, fmtPos + 4)
                            avgBytesPerSec = BitConverter.ToInt32(_data, fmtPos + 8)
                            bitsPerSample = BitConverter.ToInt16(_data, fmtPos + 14)
                        End If

                    ElseIf id0 = AscW("d"c) AndAlso id1 = AscW("a"c) AndAlso
                           id2 = AscW("t"c) AndAlso id3 = AscW("a"c) Then

                        dataBytes = CInt(subSize)
                    End If

                    pos += 8 + CInt(subSize)
                End While

                Dim info As New WavInfo()
                info.Index = wavIndex
                info.Offset = i
                info.Length = fileEnd - i
                info.SampleRate = sampleRate
                info.Channels = channels
                info.BitsPerSample = bitsPerSample

                If dataBytes > 0 AndAlso avgBytesPerSec > 0 Then
                    info.DurationSeconds = CDbl(dataBytes) / CDbl(avgBytesPerSec)
                Else
                    info.DurationSeconds = 0.0R
                End If

                _wavList.Add(info)
                ComboBox2.Items.Add(info.ToString())

                wavIndex += 1


                i = fileEnd
            Else
                i += 1
            End If
        End While
    End Sub

    Private Sub ExtractWavsToFolder()
        If _wavList Is Nothing OrElse _wavList.Count = 0 OrElse _data Is Nothing Then
            Return
        End If

        Dim baseDir As String = Path.GetDirectoryName(_actPath)
        Dim folderName As String = Path.GetFileNameWithoutExtension(_actPath) & "_audio"
        Dim outDir As String = Path.Combine(baseDir, folderName)

        Directory.CreateDirectory(outDir)

        For Each w As WavInfo In _wavList
            Dim outPath As String = Path.Combine(outDir,
                                                 String.Format("sound_{0:000}.wav", w.Index))
            Dim buf As Byte() = GetBytesSlice(w.Offset, w.Length)
            File.WriteAllBytes(outPath, buf)
        Next
    End Sub

    Private Sub PlayWav(ByVal info As WavInfo)
        Dim tempPath As String = Path.Combine(Path.GetTempPath(),
                                              "act_sound_" & info.Index.ToString() & ".wav")
        File.WriteAllBytes(tempPath, GetBytesSlice(info.Offset, info.Length))
        Dim sp As New System.Media.SoundPlayer(tempPath)
        sp.Play()
    End Sub

    Private Function GetBytesSlice(ByVal offset As Integer, ByVal length As Integer) As Byte()
        Dim buf As Byte() = New Byte(length - 1) {}
        Array.Copy(_data, offset, buf, 0, length)
        Return buf
    End Function


    Private Shared Function ReadUInt16(data As Byte(), offset As Integer) As UShort
        Return BitConverter.ToUInt16(data, offset)
    End Function

    Private Shared Function ReadUInt32(data As Byte(), offset As Integer) As UInteger
        Return BitConverter.ToUInt32(data, offset)
    End Function

    Private Shared Function IndexOfBytes(haystack As Byte(),
                                         needle As Byte(),
                                         startIndex As Integer) As Integer
        Dim limit As Integer = haystack.Length - needle.Length

        For i As Integer = startIndex To limit
            Dim found As Boolean = True
            For j As Integer = 0 To needle.Length - 1
                If haystack(i + j) <> needle(j) Then
                    found = False
                    Exit For
                End If
            Next
            If found Then
                Return i
            End If
        Next

        Return -1
    End Function

    Private Sub TextBox2_TextChanged(sender As Object, e As EventArgs) Handles TextBox2.TextChanged

    End Sub

    Private Sub Label1_Click(sender As Object, e As EventArgs) Handles Label1.Click

    End Sub
End Class
