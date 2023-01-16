$regfile = "m32def.dat"
$crystal = 11059200
$baud = 9600

'Config USARTS
Config Com1 = Dummy , Parity = None , Stopbits = 1 , Databits = 8
Open "comc.6:9600,8,e,1" For Output As #1
Open "comc.7:9600,8,e,1" For Input As #2

'Config TWI
Config Scl = Portc.0
Config Sda = Portc.1

'Config IO
Config Portc.2 = Output
Config Portc.3 = Output
Config Portc.4 = Input
Config Portc.5 = Input
Config Portd.4 = Output
Config Portd.5 = Input
Config Portd.6 = Output
Config Porta.2 = Output
Config Porta.3 = Output
Config Portb.1 = Output
Config Portb.2 = Output
Config Portb.3 = Output
Config Portb.4 = Output

'Translate IO
_selector_rs_0 Alias Portc.2
_selector_rs_1 Alias Portc.3
_stop_key Alias Pinc.4
_start_key Alias Pinc.5
_gsm_module_power Alias Portd.4
_gsm_module_vdd_detect Alias Pind.5
_regulator_power Alias Portd.6
_start_trigger Alias Porta.2
_stop_trigger Alias Porta.3
_rssi_1 Alias Portb.4
_rssi_2 Alias Portb.3
_rssi_3 Alias Portb.2
_rssi_4 Alias Portb.1

'Config built-in signals
Enable Interrupts
Config Watchdog = 2048
Enable Urxc
On Urxc Rxd1_interruption
Config Timer1 = Timer , Prescale = 1024
Enable Timer1
On Timer1 Timer1_tick
Config Int0 = Falling
Enable Int0
On Int0 Gsm_module_ringging

'Declare Subs
Declare Sub Initialize_variables
Declare Sub Reset_timer
Declare Sub Set_gsm_settings
Declare Sub Check_gsm_registeration
Declare Sub Check_signal_status
Declare Sub Power_on_gsm_module
Declare Sub Select_rs485_1
Declare Sub Select_rs485_2
Declare Sub Read_wmeter
Declare Sub Send_message(_destination_address As String * 10 , _message_text As String * 60)
Declare Sub Main_function
Declare Sub Start_trigger_activate
Declare Sub Stop_trigger_activate
Declare Sub Check_for_key
Declare Sub Check_for_card
Declare Sub Check_for_gsm_interruption
Declare Sub Read_received_sms
Declare Sub Clear_timer
Declare Sub Store_last_value
Declare Sub Display_signal_quality(byval _strength As Byte)
Declare Sub Adjust_gsm_module_time(byval _yy As Byte , Byval _mm As Byte , Byval _dd As Byte , Byval _hh As Byte , Byval _mm As Byte , Byval _ss As Byte , Byval _zz As Byte)

'Declare Functions
Declare Function Make_response_messsage(byval _response_type As Byte) As String * 100
Declare Function Get_wmeter_value() As Dword
Declare Function Is_irrigation_allowed() As Byte
Declare Function Is_gsm_module_power_on() As Byte
Declare Function Sms_sender_authentication(_message1 As String * 500) As Byte
Declare Function Extract_request_message(_message2 As String * 500) As String * 500
Declare Function Inspect_request_message(_message3 As String * 500 , _owner As Byte) As Byte
Declare Function Extract_inserted_card_serial() As Dword
Declare Function Inserted_card_validation(_card_serial As Dword) As Byte

'Declare Variables
Dim _cards_serial(51) As Eram Dword
Dim _cards_charge(51) As Eram Dword
Dim _valid_sender(5) As Eram String * 10

Dim _main_function_process_interval As Byte
Dim _waiting_time_for_empty_pipe As Byte
Dim _inserted_card_index As Byte
Dim _inserted_card_serial As Dword
Dim _temp_start_value As Dword
Dim _temp_final_value As Dword
Dim _is_irrigating As Byte
Dim _wmeter_forward_value As Dword
Dim _wmeter_flowrate As Dword
Dim _wmeter_last_forward_value As Dword
Dim _timer_counter As Byte
Dim _zeroflow_counter As Byte
Dim _rxd1_buffer As String * 500
Dim _rxd1_udr As Byte
Dim _gsm_module_ring_exist As Bit
Dim _rxd2_buffer(10) As Byte
Dim _card_list_page As Byte

Dim _i As Byte
Dim _j As Byte
Dim _k As Byte
Dim _l As Byte
Dim _x As Byte
Dim _y As Byte

Dim _tb1 As Byte
Dim _tb2 As Byte
Dim _tb3 As Byte
Dim _tb4 As Byte
Dim _tb5 As Byte
Dim _tb6 As Byte
Dim _tdw1 As Dword
Dim _tdw2 As Dword
Dim _tdw3 As Dword
Dim _tdw4 As Dword
Dim _ts1 As String * 2
Dim _ts2 As String * 10
Dim _ts3 As String * 500
Dim _ts4 As String * 500
Dim _ts5 As String * 10
Dim _tba(8) As Byte

Main:
   Stop Timer1
   Initialize_variables
   Select_rs485_1
   Stop_trigger_activate
   _tb1 = Is_gsm_module_power_on()
   If _tb1 = 0 Then
      Set _regulator_power
      Wait 1
      Power_on_gsm_module
      Set_gsm_settings
      _ts3 = Make_response_messsage(3)
      _ts2 = _valid_sender(2)
      Call Send_message(_ts2 , _ts3)
   End If
   Do
      Incr _k
      If _k > 253 Then
         Check_signal_status
         Check_gsm_registeration
         _k = 1
   End If
      Main_function
      Waitms 50
      Check_for_card
      Waitms 50
      Check_for_key
      Waitms 50
      Check_for_gsm_interruption
      Waitms 50
   Loop


Sub Initialize_variables
   _valid_sender(1) = "9152050963"
   _main_function_process_interval = 48
   _waiting_time_for_empty_pipe = 1
   _inserted_card_index = 0
   _inserted_card_serial = 0
   _temp_start_value = 0
   _temp_final_value = 0
   _is_irrigating = 0
   _timer_counter = 0
   _zeroflow_counter = 0
   _rxd1_buffer = ""
   _wmeter_forward_value = 0
   _wmeter_flowrate = 0
   _rxd1_udr = 0
   _gsm_module_ring_exist = 0
   _card_list_page = 1
   Reset _gsm_module_ring_exist
   _i = 0
   _j = 0
   _k = 0
   _l = 254
   _x = 0
   _y = 1
   _tb1 = 0
   _tb2 = 0
   _tb3 = 0
   _tb4 = 0
   _tb5 = 0
   _tb6 = 0
   _tdw1 = 0
   _tdw2 = 0
   _tdw3 = 0
   _tdw4 = 0
   _ts1 = ""
   _ts2 = ""
   _ts3 = ""
   _ts4 = ""
   _ts5 = ""
End Sub


Function Is_gsm_module_power_on() As Byte
   Is_gsm_module_power_on = 0
   If _gsm_module_vdd_detect = 0 Then
      Is_gsm_module_power_on = 1
   End If
End Function


Sub Power_on_gsm_module
   Set _gsm_module_power
   Waitms 1500
   Reset _gsm_module_power
   Wait 10
End Sub


Sub Set_gsm_settings
   Print "ATE0"
   Waitms 200
   Print "AT+IPR=9600"                                      'set baudrate
   Waitms 200
   Print "AT+CMGF=1"                                        'set baudrate
   Waitms 200
   Print "AT+CREG=1"                                        'set registeration at Home
   Waitms 200
   Print "AT+CSCS=" ; Chr(34) ; "GSM" ; Chr(34)             'set SMS character set
   Waitms 200
   Print "AT&W"
   Waitms 200
   Print "AT+CSMP=17,255,0,0"                               'set SMS coding
   Waitms 200
   Print "AT+CMGDA=" ; Chr(34) ; "DEL ALL" ; Chr(34)        'delete all messages
   Waitms 1000
   _rxd1_buffer = ""
End Sub


Sub Check_signal_status
   Print "AT+CSQ"                                           'check signal quality
   Waitms 1000
   _tb5 = Instr(_rxd1_buffer , "+CSQ: ")
   If _tb5 <> 0 Then
      _tb5 = _tb5 + 6
      _tb6 = Instr(_rxd1_buffer , ",")
      _tb6 = _tb6 - _tb5
      _ts2 = Mid(_rxd1_buffer , _tb5 , _tb6)
      _tb5 = Val(_ts2)
      Dim _received_signal_strength As Byte
      _received_signal_strength = 0
      If _tb5 > 12 And _tb5 < 17 Then
         _received_signal_strength = Bits(1)
      Elseif _tb5 > 16 And _tb5 < 22 Then
         _received_signal_strength = Bits(1 , 2)
      Elseif _tb5 > 21 And _tb5 < 26 Then
         _received_signal_strength = Bits(1 , 2 , 3)
      Elseif _tb5 > 25 Then
         _received_signal_strength = Bits(1 , 2 , 3 , 4)
      End If
      Call Display_signal_quality(_received_signal_strength)
   End If
   _rxd1_buffer = ""
End Sub


Sub Check_gsm_registeration
   Print "AT+CREG?"                                         'check registeration status
   Waitms 1000
   _tb5 = Instr(_rxd1_buffer , "+CREG: ")
   _tb5 = _tb5 + 7
   _ts2 = Mid(_rxd1_buffer , _tb5 , 3)
   If _ts2 <> "1,1" Then
      If _y < 4 Then
         Incr _y
         Waitms 500
         Check_gsm_registeration
      Else
         _y = 1
         Print "AT+CFUN=1,1"
         Wait 10
      End If
   End If
   _rxd1_buffer = ""
End Sub


Function Make_response_messsage(byval _response_type As Byte) As String * 100
   Make_response_messsage = ""
   _tb2 = 0
   _tb3 = 0
   _tdw1 = 0
   _tdw2 = 0
   _tdw3 = 0
   _ts1 = ""
   _ts5 = ""
   Select Case _response_type
      Case 1
         Make_response_messsage = "E5"
      Case 2
         Make_response_messsage = "F2"
         For _j = 1 To 5
            _ts1 = Str(_j)
            _ts5 = _valid_sender(_j)
            Make_response_messsage = Make_response_messsage + "N" + _ts1 + "(" + _ts5 + ")"
         Next _j
      Case 3
         Select_rs485_2
         Read_wmeter
         Select_rs485_1
         _tdw1 = _cards_serial(_inserted_card_index)
         _ts5 = Str(_tdw1)
         Make_response_messsage = "F1(" + _ts5 + ")S1("
         _tb3 = _is_irrigating
         _ts5 = Str(_tb3)
         Make_response_messsage = Make_response_messsage + _ts5 + ")V1("
         _ts5 = Str(_wmeter_forward_value)
         _ts5 = Format(_ts5 , "0.0")
         Make_response_messsage = Make_response_messsage + _ts5 + ")V2("
         _ts5 = Str(_wmeter_flowrate)
         Make_response_messsage = Make_response_messsage + _ts5 + ")V3("
         _tdw2 = _temp_final_value
         If _tdw2 <= _wmeter_forward_value Then
            _ts5 = "0"
         Else
            _tdw3 = _tdw2 - _wmeter_forward_value
            If _tdw3 <> 0 Then
               _ts5 = Str(_tdw3) + "00"
            End If
         End If
         Make_response_messsage = Make_response_messsage + _ts5 + ")"
      Case 4
         _tb2 = _card_list_page * 10
         _tb3 = _tb2 - 9
         For _j = _tb3 To _tb2
            _ts1 = Str(_j)
            Make_response_messsage = Make_response_messsage + "C(" + _ts1 + ")N("
            _tdw2 = _cards_serial(_j)
            _ts5 = Str(_tdw2)
            Make_response_messsage = Make_response_messsage + _ts5 + "):" + Chr(13) + Chr(10)
            _tdw2 = _cards_charge(_j)
            _ts5 = Str(_tdw2)
            Make_response_messsage = Make_response_messsage + _ts5 + " lit" + Chr(13) + Chr(10)
         Next _j
      Case 5
         Make_response_messsage = "Error"
      Case 6
         _tdw1 = _cards_serial(_inserted_card_index)
         _ts5 = Str(_tdw1)
         Make_response_messsage = "F1(" + _ts5 + ")V3("
         _tdw3 = _cards_charge(_inserted_card_index)
         _ts5 = Str(_tdw3)
         Make_response_messsage = Make_response_messsage + _ts5 + ")V4("
         _tdw4 = _wmeter_forward_value - _temp_start_value
         _ts5 = Str(_tdw4) + "00"
         Make_response_messsage = Make_response_messsage + _ts5 + ")"
      Case 7
         _ts1 = Str(_k)
         Make_response_messsage = Make_response_messsage + "C(" + _ts1 + "):" + Chr(13) + Chr(10)
         _tdw2 = _cards_charge(_k)
         _ts5 = Str(_tdw2)
         Make_response_messsage = Make_response_messsage + _ts5 + " lit"
   End Select
End Function


Sub Send_message(_destination_address As String * 10 , _message_text As String * 60)
   _ts2 = _destination_address
   Print "AT+CSMP=17,255,0,0"                               'set SMS coding
   Waitms 200
   Print "AT+CMGS=" ; Chr(34) ; Chr(48) ; _ts2 ; Chr(34)    'send sms
   Waitms 100
   Print _message_text ; Chr(26)
   Wait 8
   _rxd1_buffer = ""
End Sub


Sub Main_function
   _tb2 = 0
   _tdw1 = 0
   _ts2 = ""
   _ts3 = ""
   _tb2 = _main_function_process_interval
   If _timer_counter >= _tb2 Then                           'set read interval Tb*5 seconds
      Select_rs485_2
      Read_wmeter
      Select_rs485_1
      If _wmeter_last_forward_value = _wmeter_forward_value Then
         Incr _zeroflow_counter
         _tb2 = _waiting_time_for_empty_pipe
         If _zeroflow_counter >= _tb2 Then
            Call Stop_trigger_activate
            _ts3 = Make_response_messsage(3)
            _ts2 = _valid_sender(2)
            Call Send_message(_ts2 , _ts3)
         End If
      Else
         _zeroflow_counter = 0
         _tdw1 = _temp_final_value
         If _wmeter_forward_value > _tdw1 Then
            Call Stop_trigger_activate
            Waitms 4
            _ts3 = Make_response_messsage(3)
            _ts2 = _valid_sender(2)
            Call Send_message(_ts2 , _ts3)
         Else
            Store_last_value
            _wmeter_last_forward_value = _wmeter_forward_value
         End If
      End If
      _timer_counter = 0
   End If
End Sub


Sub Check_for_card
   _tb1 = 0
   _tb3 = 0
   _ts2 = ""
   _ts3 = ""
   If _is_irrigating = 0 Then
      _inserted_card_serial = 0
      _inserted_card_serial = Extract_inserted_card_serial()
      If _inserted_card_serial <> 0 Then
         _tb1 = Inserted_card_validation(_inserted_card_serial)
         If _tb1 <> 0 Then
            _inserted_card_index = _tb1
            If Is_irrigation_allowed() = 1 Then
               Start_trigger_activate
               _tb3 = 1
            End If
         End If
      End If
   End If
End Sub


Sub Check_for_key
   _tb3 = 0
   _ts2 = ""
   _ts3 = ""
   If _stop_key = 0 And _is_irrigating = 1 Then
      Waitms 1
      If _stop_key = 0 Then
            Stop_trigger_activate
            _tb3 = 1
      End If
   End If
   If _tb3 = 1 Then
      _ts3 = Make_response_messsage(6)
      _ts2 = _valid_sender(2)
      Call Send_message(_ts2 , _ts3)
   End If
End Sub


Sub Check_for_gsm_interruption
   If _gsm_module_ring_exist = 1 Then
      Read_received_sms
      _gsm_module_ring_exist = 0
   End If
End Sub


Sub Start_trigger_activate
   _temp_start_value = _wmeter_forward_value
   Set _start_trigger
   Wait 1
   Reset _start_trigger
   Clear_timer
   _is_irrigating = 1
End Sub


Sub Stop_trigger_activate
   Set _stop_trigger
   Wait 1
   Reset _stop_trigger
   Stop Timer1
   _is_irrigating = 0
   Wait 1
   Select_rs485_2
   Read_wmeter
   Select_rs485_1
   Waitms 500
   Store_last_value
End Sub


Function Extract_inserted_card_serial() As Dword
   Extract_inserted_card_serial = 0
   _ts2 = ""
   For _i = 1 To 10
       _rxd2_buffer(_i) = 0
   Next _i
   Printbin #1 , &H01
   $timeout = 500000
   Inputbin #2 , _rxd2_buffer(1) , 10
   _ts2 = Chr(_rxd2_buffer(1)) + Chr(_rxd2_buffer(2)) + Chr(_rxd2_buffer(3)) + Chr(_rxd2_buffer(4)) + Chr(_rxd2_buffer(5))
   _ts2 = _ts2 + Chr(_rxd2_buffer(6)) + Chr(_rxd2_buffer(7)) + Chr(_rxd2_buffer(8)) + Chr(_rxd2_buffer(9)) + Chr(_rxd2_buffer(10))
   Extract_inserted_card_serial = 0
   _ts2 = Mid(_ts2 , 5 , 6 )
   Extract_inserted_card_serial = Hexval(_ts2)
End Function


Function Inserted_card_validation(_card_serial As Dword) As Byte
   Inserted_card_validation = 0
   _tdw3 = 0
   For _j = 1 To 100
      _tdw3 = _cards_serial(_j)
      If _card_serial = _tdw3 Then
         Inserted_card_validation = _j
         Exit For
      End If
   Next _j
End Function


Sub Read_wmeter
   For _i = 1 To 7
      _rxd2_buffer(_i) = 0
   Next _i
   _tba(1) = &H01                                           ' read water meter positive value
   _tba(2) = &H03
   _tba(3) = &H02
   _tba(4) = &H00
   _tba(5) = &H00
   _tba(6) = &H02
   _tba(7) = &HC5
   _tba(8) = &HB3
   Printbin #1 , _tba(1) , 8
   $timeout = 1000000
   Inputbin #2 , _rxd2_buffer(1) , 7
   Waitms 100
   _tdw1 = Get_wmeter_value()
   If _x = 0 Then
      _tdw4 = _tdw1
      Incr _x
      Waitms 5
      Read_wmeter
   End If
   If _tdw1 < _tdw4 Then
      _tdw4 = _tdw4 - _tdw1
   Else
      _tdw4 = _tdw1 - _tdw4
   End If
   If _tdw4 < 10 Then
      _wmeter_forward_value = _tdw1
   End If
   _x = 0
End Sub


Sub Select_rs485_1
   Reset _selector_rs_0
   Reset _selector_rs_1
End Sub


Sub Select_rs485_2
   Reset _selector_rs_0
   Set _selector_rs_1
End Sub


Function Get_wmeter_value() As Dword
   Get_wmeter_value = 0
   Local Tdw_11 As Dword
   Local Tdw_12 As Dword
   Tdw_11 = 0
   Tdw_12 = 0
   Tdw_11 = 16777216 * _rxd2_buffer(4)
   Tdw_12 = 65536 * _rxd2_buffer(5)
   Tdw_12 = Tdw_12 + Tdw_11
   Tdw_11 = 256 * _rxd2_buffer(6)
   Tdw_12 = Tdw_12 + Tdw_11
   Tdw_12 = Tdw_12 + _rxd2_buffer(7)
   Get_wmeter_value = Tdw_12
End Function


Sub Read_received_sms
   _ts3 = ""
   _ts1 = ""
   _ts3 = _rxd1_buffer
   _rxd1_buffer = ""
   _tb1 = Instr(_ts3 , "SM")
   _tb1 = _tb1 + 4
   _ts1 = Mid(_ts3 , _tb1 , 1)
   _ts3 = ""
   Print "AT+CMGR=" ; _ts1                                  ' read sms
   Wait 2
   _ts3 = _rxd1_buffer
   _rxd1_buffer = ""
   _tb1 = 0
   _tb1 = Sms_sender_authentication(_ts3)
   If _tb1 <> 0 Then
      _ts3 = Extract_request_message(_ts3)
      _tb2 = Inspect_request_message(_ts3 , _tb1)
      If _tb2 <> 0 Then
         _ts3 = Make_response_messsage(_tb2)
         _ts2 = ""
         _ts2 = _valid_sender(_tb1)
         Call Send_message(_ts2 , _ts3)
      End If
   End If
   Print "AT+CMGDA=" ; Chr(34) ; "DEL ALL" ; Chr(34)
   Wait 2
   _rxd1_buffer = ""
End Sub


Function Sms_sender_authentication(_message1 As String * 500) As Byte
   Sms_sender_authentication = 0
   _tb2 = 0
   _ts2 = ""
   For _j = 1 To 5
      _tb2 = 0
      _ts2 = _valid_sender(_j)
      _tb2 = Instr(_message1 , _ts2)
      If _tb2 <> 0 And _tb2 < 30 Then
         Sms_sender_authentication = _j
         Exit For
      End If
   Next _j
End Function


Function Extract_request_message(_message2 As String * 500) As String * 500
   _tb4 = 0
   _ts1 = ""
   Extract_request_message = Mid(_message2 , 66 , 200 )
   _ts1 = Chr(13)
   _tb4 = Instr(extract_request_message , _ts1)
   Extract_request_message = Left(extract_request_message , _tb4)
End Function


Function Inspect_request_message(_message3 As String * 500 , _owner As Byte) As Byte
   Inspect_request_message = 0
   _tb3 = 0
   _tb4 = 0
   _tb5 = 0
   _tdw3 = 0
   _ts1 = ""
   _ts2 = ""
   _ts4 = ""
   _Ts4 = _message3
   _tb3 = _owner
   _ts1 = Mid(_ts4 , 1 , 2)
   If _ts1 = "68" And _tb3 < 3 Then
      _ts1 = Mid(_ts4 , 3 , 2)
      If _ts1 = "57" Then
         _tb4 = Instr(_ts4 , "F2")
         If _tb4 <> 0 Then
            _tb5 = Instr(_ts4 , "N2")
            If _tb5 <> 0 Then
               _tb5 = _tb5 + 2
               _ts2 = Mid(_ts4 , _tb5 , 10)
               _valid_sender(2) = _ts2
               Waitms 4
               Inspect_request_message = 1
            End If
            _tb5 = Instr(_ts4 , "N3")
            If _tb5 <> 0 Then
               _tb5 = _tb5 + 2
               _ts2 = Mid(_ts4 , _tb5 , 10)
               _valid_sender(3) = _ts2
               Waitms 4
               Inspect_request_message = 1
            End If
            _tb5 = Instr(_ts4 , "N4")
            If _tb5 <> 0 Then
               _tb5 = _tb5 + 2
               _ts2 = Mid(_ts4 , _tb5 , 10)
               _valid_sender(4) = _ts2
               Waitms 4
               Inspect_request_message = 1
            End If
            _tb5 = Instr(_ts4 , "N5")
            If _tb5 <> 0 Then
               _tb5 = _tb5 + 2
               _ts2 = Mid(_ts4 , _tb5 , 10)
               _valid_sender(5) = _ts2
               Waitms 4
               Inspect_request_message = 1
            End If
         End If
      Elseif _ts1 = "52" Then
         _ts1 = Mid(_ts4 , 5 , 2)
         If _ts1 = "49" Then
            Inspect_request_message = 2
         End If
      End If
   Elseif _ts1 = "CC" And _tb3 < 4 Then
      _k = 0
      _ts1 = Mid(_ts4 , 3 , 2)
      _k = Val(_ts1)
      If _k < 51 Then
         _tb4 = Instr(_ts4 , "NN")
         If _tb4 <> 0 Then
            _tb4 = _tb4 + 2
            _ts2 = Mid(_ts4 , _tb4 , 10)
            _tdw3 = Val(_ts2)
            _cards_serial(_k) = _tdw3
            Waitms 4
            Inspect_request_message = 1
         End If
         _tb4 = Instr(_ts4 , "VV")
         If _tb4 <> 0 And _is_irrigating = 0 Then
            _tb4 = _tb4 + 2
            _ts2 = Mid(_ts4 , _tb4 , 6)
            If _ts2 <> Chr(13) Then
               _tdw3 = Val(_ts2)
               _cards_charge(_k) = _tdw3
               Waitms 4
               Inspect_request_message = 7
            End If
         Else
            _tb4 = Instr(_ts4 , "SS")
            If _tb4 <> 0 And _is_irrigating = 0 Then
               _tb4 = _tb4 + 2
               _ts2 = Mid(_ts4 , _tb4 , 6)
               If _ts2 <> Chr(13) Then
                  _tdw3 = Val(_ts2)
                  If _tdw3 > 0 Then
                     _tdw4 = _cards_charge(_k) + _tdw3
                     _cards_charge(_k) = _tdw4
                     Waitms 4
                     Inspect_request_message = 7
                  End If
               End If
            Elseif _is_irrigating = 1 Then
               Inspect_request_message = 5
            End If
         End If
      End If
   Elseif _ts1 = "10" Then
      _ts1 = Mid(_ts4 , 3 , 2)
      If _ts1 = "45" Then
         _ts1 = Mid(_ts4 , 5 , 2)
         Select Case _ts1
            Case "00"
               Call Stop_trigger_activate
               Inspect_request_message = 3
            Case "01"
               If _is_irrigating = 0 Then
                  _ts2 = Mid(_ts4 , 7 , 4)
                  If _ts2 <> Chr(13) Then
                     Select_rs485_2
                     Read_wmeter
                     Select_rs485_1
                     _tdw3 = Val(_ts2)
                     _tdw3 = _tdw3 / 100
                     _tdw3 = _tdw3 + _wmeter_forward_value
                     _temp_final_value = _tdw3
                     Call Start_trigger_activate
                     _tdw3 = _tb3
                     _inserted_card_index = 51
                     _cards_serial(_inserted_card_index) = _tdw3
                     Waitms 4
                     Inspect_request_message = 3
                  End If
               Else
                  Inspect_request_message = 5
               End If
            Case "52"
               Start Watchdog
               Waitms 2200
            Case "FF"
               Inspect_request_message = 3
         End Select
      End If
   Elseif _ts1 = "52" And _tb3 < 4 Then
      _ts1 = Mid(_ts4 , 3 , 2)
      If _ts1 = "PP" Then
         _ts1 = Mid(_ts4 , 5 , 2)
         Inspect_request_message = 4
         Select Case _ts1
            Case "01"
               _card_list_page = 1
            Case "02"
               _card_list_page = 2
            Case "03"
               _card_list_page = 3
            Case "04"
               _card_list_page = 4
            Case "05"
               _card_list_page = 5
         End Select
      Elseif _ts1 = "CC" Then
         _ts1 = Mid(_ts4 , 5 , 2)
         _k = Val(_ts1)
         Inspect_request_message = 7
      End If
   End If
End Function


Sub Store_last_value
   _tdw1 = 0
   _tdw2 = 0
   If _temp_final_value > _wmeter_forward_value Then
      _tdw1 = _temp_final_value - _wmeter_forward_value
      _tdw1 = _tdw1 * 100
      _tdw2 = _cards_charge(_inserted_card_index)
      If _tdw1 < _tdw2 Then
         _cards_charge(_inserted_card_index) = _tdw1
         Waitms 4
      End If
   Else
      _cards_charge(_inserted_card_index) = 0
   End If
End Sub


Sub Display_signal_quality(byval _strength As Byte)
   _rssi_1 = _strength.1
   _rssi_2 = _strength.2
   _rssi_3 = _strength.3
   _rssi_4 = _strength.4
End Sub


Sub Adjust_gsm_module_time(byval _yy As Byte , Byval _mm As Byte , Byval _dd As Byte , Byval _hh As Byte , Byval _mm As Byte , Byval _ss As Byte , Byval _zz As Byte)

End Sub


Function Is_irrigation_allowed() As Byte
   Is_irrigation_allowed = 0
   _tdw2 = 0
   Select_rs485_2
   Read_wmeter
   Select_rs485_1
   _tdw2 = _cards_charge(_inserted_card_index)
   _tdw2 = _tdw2 / 100
   If _tdw2 > 0 And _tdw2 < 10000 Then
      _temp_final_value = _wmeter_forward_value + _tdw2
      Is_irrigation_allowed = 1
   End If
End Function


Sub Clear_timer
   _timer_counter = 0
   _zeroflow_counter = 0
   Reset_timer
End Sub


Sub Reset_timer
   Timer1 = 11536                                           'Set timer for 5 second
   Start Timer1
End Sub


Timer1_tick:
   Stop Timer1
   Incr _timer_counter
   Reset_timer
Return


Rxd1_interruption:
   _rxd1_udr = Udr
   _rxd1_buffer = _rxd1_buffer + Chr(_rxd1_udr)
Return


Gsm_module_ringging:
   Set _gsm_module_ring_exist
Return












