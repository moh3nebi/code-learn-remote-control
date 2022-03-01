'**************************************
'LEARNING REMOTE DECODER
' Program by Mohsen Ebrahimi
' mohsen.ebrahimi156@gmail.com
'**************************************
$regfile = "M8adef.dat"
$crystal = 4000000
'-------------------------------------------------------------------------------
Config Pinb.0 = Input                                       'RF INPUT
Config Pinc.5 = Output                                      'Buzzer B.1
Config Pind.2 = Output                                      'relay 1
Config Pind.3 = Output                                      'relay 2
Config Pind.4 = Output                                      'relay3
Config Pind.5 = Output                                      'relay4
Config Pinc.4 = Output                                      'led1 learning led
Config Pinc.0 = Input                                       'key1
Config Scl = Portb.2                                        'at24cxx pin6
Config Sda = Portb.1                                        'at24cxx pin5
'--------------------------------- Alias  --------------------------------------
_in Alias Pinb.0                                            'RF input
Buzz Alias Portc.5                                          'B.1
Rel1 Alias Portd.2                                          'relay1
Rel2 Alias Portd.3                                          'relay2
Rel3 Alias Portd.4                                          'relay3
Rel4 Alias Portd.5                                          'relay4
Led1 Alias Portc.4                                          'learning led
Key1 Alias Pinc.0                                           'learn key
''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
Const Eewrite = 160                                         'eeprom write address
Const Eeread = 161                                          'eeprom read address
'--------------------------------- Timer ---------------------------------------
Config Timer1 = Timer , Prescale = 8 : Stop Timer1 : Timer1 = 0
'Config Watchdog = 2048
'--------------------------------- Variable ------------------------------------
Dim S(24)as Word
Dim I As Byte
I = 0
Dim Saddress As String * 20
Dim Scode As String * 4
Dim Address As Long
Dim Code As Byte
''''''''''''''''''''''''''''''''
Dim Ra As Long                                              'fp address
Dim Rnumber As Byte                                         'remote know
Dim Okread As Bit
Dim Error As Bit
Dim Keycheck As Bit
Dim T As Word                                               'check for pushing lean key time
Error = 0
Okread = 0
T = 0
Keycheck = 0
Dim Eaddress As Word                                        'eeprom address variable
Dim E_read As Byte
Dim E_write As Byte
Dim M As String * 8
Dim M1 As String * 2
Dim M2 As String * 2
Dim M3 As String * 2
'-------------------------- read rnumber index from eeprom
Gosub Rnumber_er
If Rnumber > 100 Then
Rnumber = 0
Gosub Rnumber_ew
End If
'------------------- startup
Waitms 500
Set Led1
Gosub Beep
Gosub Beep
Reset Led1
Waitms 500
Enable Interrupts

Main:
'Start Watchdog
'************************************************************************ main
Do
'Reset Watchdog
Gosub _read
If Key1 = 0 Then
'Reset Watchdog
'Stop Watchdog
Gosub Beep
Waitms 100
Gosub Keys
'Start Watchdog
End If

Loop
'*******************************************************************************
'--------------------------------------------------------------------------read
_read:
Okread = 0
If _in = 1 Then
Do
'Reset Watchdog
If _in = 0 Then Exit Do
Loop
Timer1 = 0
Start Timer1
While _in = 0
'Reset Watchdog
Wend
Stop Timer1
If Timer1 >= 3500 And Timer1 <= 8800 Then
Do
If _in = 1 Then
Timer1 = 0
Start Timer1
While _in = 1
'Reset Watchdog
Wend
Stop Timer1
Incr I
S(i) = Timer1
End If
'Reset Watchdog
If I = 24 Then Exit Do
Loop
For I = 1 To 24
'Reset Watchdog
If S(i) >= 120 And S(i) <= 350 Then
S(i) = 0
Else
If S(i) >= 400 And S(i) <= 850 Then
 S(i) = 1
 Else
 I = 0
 Address = 0
 Code = 0
 Okread = 0
 Return
 End If
 End If
Next
I = 0
Saddress = ""
Scode = ""
For I = 1 To 20
Saddress = Saddress + Str(s(i))
Next
For I = 21 To 24
Scode = Scode + Str(s(i))
Next
Address = Binval(saddress)
Code = Binval(scode)
Gosub Check
''''''''''''''''''''''''''''''''''''''''''
I = 0
End If
End If
Return

'========================================================================= keys  learning
Keys:
Reset Rel1
Reset Rel2
Reset Rel3
Reset Rel4
Set Led1
Keycheck = 1                                                'hengame learn kardan be releha farman nade
Waitms 150
Do
If Key1 = 0 Then                                            ' agar kelid feshorde bemanad
Gosub Beep
While Key1 = 0
Incr T
Waitms 1
If T >= 5000 Then
T = 0
Gosub Beep
Rnumber = 0
Gosub Rnumber_ew
Set Buzz
Wait 1
Wait 1
Reset Buzz
Reset Led1
Return
Exit While
End If
Wend
If T < 5000 Then
T = 0
Reset Led1
Return
End If
End If
''''''''''''''''''''''''''^^^
Gosub _read
If Okread = 1 Then
Gosub Beep
''''''''''''''''''''''repeat check
If Rnumber = 0 Then                                         ' agar avalin remote as ke learn mishavad
Incr Rnumber
Gosub Rnumber_ew
Ra = Address
Gosub Ra_w
Exit Do
   Else
Eaddress = 10                                               'address avalin khane baraye zakhire address remote
For I = 1 To Rnumber
Gosub Ra_r
If Ra = Address Then                                        'agar address remote tekrari bod yani ghablan learn shode
Set Buzz
Wait 1
Reset Buzz
Error = 1
Exit For
Else
Error = 0
End If
Eaddress = Eaddress + 1
Next
If Error = 0 Then                                           ' agar tekrari nabod
Incr Rnumber                                                'be meghdare rnumber ke index tedade remote haye learn shode ast yek vahed ezafe kon
If Rnumber > 100 Then                                       'agar bishtar az 100 remote learn shavad
Rnumber = 100
Set Buzz
Wait 5
Reset Buzz
Else                                                        'agar kamtar az 100 remote bod
Gosub Rnumber_ew                                            'meghdare rnumber ra dar eeprom zakhore mikonad
Ra = Address
Gosub Ra_w
End If
End If
End If
Exit Do
End If
Loop
Okread = 0
Reset Led1
Return
'================================================================ CHECK   code chek ok
Check:
Okread = 1
If Keycheck = 0 Then                                        'agar keycheck=1 bashad yani be releha farman nade
Eaddress = 10
For I = 1 To Rnumber
Gosub Ra_r
If Ra = Address Then                                        'code
Gosub Command
Gosub Beep
Exit For
End If
Eaddress = Eaddress + 1
Next
End If
Keycheck = 0
Return
'-------------------------------- Relay command
Command:
Select Case Code:

Case 1:
Toggle Rel1
Waitms 50

Case 2:
Toggle Rel2
Waitms 50

Case 4:
Toggle Rel3
Waitms 50

Case 8:
Toggle Rel4
Waitms 50

Case Else
End Select
Return
'-------------------

'-------------------------------------------------------------------------- BEEP
Beep:
Set Buzz
Waitms 80
Reset Buzz
Waitms 30
Return
'---------------------- for write a byte to eeprom
Eew:
I2cstart
I2cwbyte Eewrite
I2cwbyte Eaddress                                           'A
I2cwbyte E_write
I2cstop
Waitms 10
Return
'''''''''''''''''''''' for read a byte to eeprom
Eer:
I2cstart
I2cwbyte Eewrite
I2cwbyte Eaddress                                           'A
I2cstart
I2cwbyte Eeread
I2crbyte E_read , Nack
I2cstop
Return

'--------------------------------- rnumber_er >eeprom read remote number learn
Rnumber_er:
Eaddress = 1
Gosub Eer
Rnumber = E_read
Return
'----------------------- rnumber_ew
Rnumber_ew:
Eaddress = 1
E_write = Rnumber
Gosub Eew
Return
'----------------------ra_w   write address code to eeprom
Ra_w:
M = ""
M = Hex(ra)
M1 = Mid(m , 3 , 2)
M2 = Mid(m , 5 , 2)
M3 = Mid(m , 7 , 2)
Gosub Decode
E_write = Hexval(m1)
Gosub Eew
Incr Eaddress
E_write = Hexval(m2)
Gosub Eew
Incr Eaddress
E_write = Hexval(m3)
Gosub Eew
Return
'----------------------ra_r  read address code from eeprom
Ra_r:
Gosub Eer
M1 = Hex(e_read)
Incr Eaddress
Gosub Eer
M2 = Hex(e_read)
Incr Eaddress
Gosub Eer
M3 = Hex(e_read)
M = ""
M = M + M1
M = M + M2
M = M + M3
Ra = Hexval(m)
Return
'------------------------------------------------------decode eeprom address
Decode:
Select Case Rnumber
Case 1:
Eaddress = 10
Case 2:
Eaddress = 13
Case 3:
Eaddress = 16
Case 4:
Eaddress = 19
Case 5:
Eaddress = 22
Case 6:
Eaddress = 25
Case 7:
Eaddress = 28
Case 8:
Eaddress = 31
Case 9:
Eaddress = 34
Case 10:
Eaddress = 37
Case 11:
Eaddress = 40
Case 12:
Eaddress = 43
Case 13:
Eaddress = 46
Case 14:
Eaddress = 49
Case 15:
Eaddress = 52
Case 16:
Eaddress = 55
Case 17:
Eaddress = 58
Case 18:
Eaddress = 61
Case 19:
Eaddress = 64
Case 20:
Eaddress = 67
Case 21:
Eaddress = 70
Case 22:
Eaddress = 73
Case 23:
Eaddress = 76
Case 24:
Eaddress = 79
Case 25:
Eaddress = 82
Case 26:
Eaddress = 85
Case 27:
Eaddress = 88
Case 28:
Eaddress = 91
Case 29:
Eaddress = 94
Case 30:
Eaddress = 97
Case 40:
Eaddress = 100
Case 41:
Eaddress = 103
Case 42:
Eaddress = 106
Case 43:
Eaddress = 109
Case 44:
Eaddress = 112
Case 45:
Eaddress = 115
Case 46:
Eaddress = 118
Case 47:
Eaddress = 121
Case 48:
Eaddress = 124
Case 49:
Eaddress = 127
Case 50:
Eaddress = 130
Case 51:
Eaddress = 133
Case 52:
Eaddress = 136
Case 53:
Eaddress = 139
Case 54:
Eaddress = 142
Case 55:
Eaddress = 145
Case 56:
Eaddress = 148
Case 57:
Eaddress = 151
Case 58:
Eaddress = 154
Case 59:
Eaddress = 157
Case 60:
Eaddress = 160
Case 70:
Eaddress = 163
Case 71:
Eaddress = 166
Case 72:
Eaddress = 169
Case 73:
Eaddress = 172
Case 74:
Eaddress = 175
Case 75:
Eaddress = 178
Case 76:
Eaddress = 181
Case 77:
Eaddress = 184
Case 78:
Eaddress = 187
Case 79:
Eaddress = 190
Case 80:
Eaddress = 193
Case 81:
Eaddress = 196
Case 82:
Eaddress = 199
Case 83:
Eaddress = 202
Case 84:
Eaddress = 205
Case 85:
Eaddress = 208
Case 86:
Eaddress = 211
Case 87:
Eaddress = 214
Case 88:
Eaddress = 217
Case 89:
Eaddress = 220
Case 90:
Eaddress = 223
Case 91:
Eaddress = 226
Case 92:
Eaddress = 229
Case 93:
Eaddress = 232
Case 94:
Eaddress = 235
Case 95:
Eaddress = 238
Case 96:
Eaddress = 241
Case 97:
Eaddress = 244
Case 98:
Eaddress = 247
Case 99:
Eaddress = 250
Case 100:
Eaddress = 253
Case Else
End Select
Return
'-------------------------------------------------------------------------------