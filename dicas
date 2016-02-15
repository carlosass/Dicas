

*************************************************************************************
Descrição de status de JOB:

CONSTANTS:
  btc_running       LIKE tbtco-status VALUE 'R',
  btc_ready         LIKE tbtco-status VALUE 'Y',
  btc_scheduled     LIKE tbtco-status VALUE 'P',
  btc_intercepted   TYPE btcstatus VALUE btc_scheduled,
  btc_released      LIKE tbtco-status VALUE 'S',
  btc_aborted       LIKE tbtco-status VALUE 'A',
  btc_finished      LIKE tbtco-status VALUE 'F',
  btc_put_active    LIKE tbtco-status VALUE 'Z',
  btc_unknown_state LIKE tbtco-status VALUE 'X'.

  CASE job_head_input-status.
    WHEN btc_running.
      btch1140-statustxt = text-077.  "ativo
    WHEN btc_ready.
      btch1140-statustxt = text-078.  "Pronto
    WHEN btc_scheduled.
      btch1140-statustxt = text-079.  "escalonado
    WHEN btc_released.
      btch1140-statustxt = text-080.  "liberado
    WHEN btc_aborted.
      btch1140-statustxt = text-081.  "cancelado
    WHEN btc_finished.
      btch1140-statustxt = text-082.  "Concl.
    WHEN btc_put_active.
      btch1140-statustxt = text-748.  "liberado/susp.
    WHEN OTHERS.
      btch1140-statustxt = text-083.  "
  ENDCASE.


*************************************************************************************
Leitura inversa de uma string
REPORT ZEX_STRINGREV .

Parameters: p_string(200).
Data: r_string(100).

CALL FUNCTION 'STRING_REVERSE'
  EXPORTING
    STRING          = p_string
    LANG            = 'E'
 IMPORTING
   RSTRING          = r_string
 EXCEPTIONS
   TOO_SMALL        = 1
   OTHERS           = 2
          .
IF SY-SUBRC <> 0.
 MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.

*Inputted String.

Write:/ 'Inputted String', p_string.
SKIP.

*The reverse of the above mentioned string is as follows.
Write:/ r_string.

*************************************************************************************
Transportar variantes para outro ambiente:
Run the program RSTRANSP and give the name of the program and the variant name.
It will create a transport request for the variant.
the rest of the process is the same as is the case with all other transport requests.
 
Or
 
Create Variant with Transport request:

Goto SE38->select variant radio button ->click on display->enter ur variant name->UTILITIES->Transport request..

*************************************************************************************
Habilitando ou desabilitando campos na ME51N/ME52N/ME22N/ME21N:
Criar enhancement no final da função MEREQBADI_FS_ITEM, com código:

DATA: WA_ITEM   TYPE MEREQ_ITEM,
      WA_header TYPE MEREQ_header.

  FIELD-SYMBOLS: <fs_field> like line of CH_FIELDSELECTION.

  CLEAR: WA_ITEM.

  "Busca na tela, dados itens ...
  CALL METHOD IM_ITEM->GET_DATA  " Get PR items
    RECEIVING
        RE_DATA = WA_ITEM.

  "Busca na tela, dados cabeçalho...
  CALL METHOD IM_header->GET_DATA  " Get PR items
    RECEIVING
        RE_DATA = WA_header.

    READ TABLE CH_FIELDSELECTION ASSIGNING <fs_field> WITH TABLE KEY METAFIELD = mmmfd_mat_grp.
    IF SY-SUBRC IS INITIAL.
      <fs_field>-fieldstatus = '.'.     '. --> abre campo pra edição       * --> somente exibição
    ENDIF.

*************************************************************************************
Criando instância objeto (instanciando):
TYPE-POOLS: mmmfd.

DATA: gf_me_commtmnt_req_relev TYPE REF TO if_ex_me_commtmnt_req_rele,
      gf_me_commtmnt_req_relev_activ(1),
      gf_me_commtmnt_req_relev_cust
                              TYPE REF TO if_ex_me_commtmnt_req_re_c,
      gf_me_commtmnt_req_rel_c_activ(1).

DATA: gf_me_process_req      TYPE REF TO if_ex_me_process_req,
      gf_me_process_req_active(1),
      gf_me_process_req_cust TYPE REF TO if_ex_me_process_req_cust,
      gf_me_process_req_cust_active(1).


data: ch_instance_cust TYPE REF TO if_ex_me_process_req_cust.

  CLEAR ch_instance_cust.
  IF gf_me_process_req_cust_active IS INITIAL.
    gf_me_process_req_cust ?= cl_badi_mm=>get_instance( 'ME_PROCESS_REQ_CUST' ).
    IF NOT gf_me_process_req_cust IS INITIAL.
      gf_me_process_req_cust_active = 'Y'.
    ELSE.
      gf_me_process_req_cust_active = 'N'.
    ENDIF.
  ENDIF.

  if not l_instance_cust is initial.
      call method l_instance_cust->fieldselection_item
        exporting
          im_header         = im_header
          im_item           = im_item
        changing
          ch_fieldselection = lt_fieldselection.

  endif.

*************************************************************************************
"Buscar um número qualquer (randômico)...
data: v_random        type i,
      v_nr_seq        type swo_typeid.

  cl_abap_random=>seed( receiving seed = v_random ).
  v_nr_seq = v_random.

*************************************************************************************
Dados complementares (depto, função, Edifício, andar, etc) de cliente:
pegar na tabela KNA1 o campo ADRNR... através deste, buscar na tabela ADCP (ADCP-ADDRNUMBER = KNA1-ADRNR), os campos ROOMNUMBER, FUNCTION, etc.

Outros dados em ADRC.
*************************************************************************************
Verificar chave de acesso de objeto:

    CALL FUNCTION 'CHECK_ACCESS_KEYS'
      EXPORTING
        developer_only  = l_only_user_dialog
        i_adir_obj      = adir_key
        suppress_dialog = abap_true
      EXCEPTIONS
        access_denied   = 1
        display_only    = 2
        OTHERS          =bal
*************************************************************************************

Transação S_ALR_87013019: Lista de ordem interna (saldo).

*************************************************************************************

Num ALV OO (orientado a objetos), considerar apenas as linhas filtradas (filtro):
No PAI (da tela):
  data: lt_fcr type lvc_t_fidx,
        wa_fltr(20) type n,
        l_tabix     type sy-tabix.

  "Buscar linhas filtradas...
  call method o_grid->get_filtered_entries
    importing
      et_filtered_entries = lt_fcr.

  loop at tg_alv into eg_alv where flag eq cg_x.

    l_tabix = sy-tabix.

    "Verificar se a linha foi filtrada ------------------------- Início
    if not lt_fcr[] is initial.
      read table lt_fcr with key table_line = l_tabix
                                 transporting no fields.
      "Se a linha da grid estiver na tabela de filtro, é porque
      "não foi filtrada.
      check sy-subrc ne 0.
    endif.
    "Verificar se a linha foi filtrada ------------------------ Fim

     ...
       ...
   endloop.

*************************************************************************************
Função determinação de datas de imobilizados (as03):
FAA_DC_ENGINE_CALL

Para alterar campos, datas imobilizado:
Criar enhancement no início da função AMSP_MODIFY_GLOBAL_TABLES. Alterar tabela interna t_anlb

*************************************************************************************
Problema com tela (layout) em views web dynpro:
Transação SICF (Atualização de serviços) --- Navegar para: default_host ... sap ... bc ... wdvd (ativar)

*************************************************************************************
Alterar idioma em tempo de execução:
  SET LOCALE LANGUAGE sy-langu.
*************************************************************************************
Programas FTP:
RSFTP002                         Execute FTP Command    
RSFTP003                         Test                   
RSFTP004                         FTP copy               
RSFTP006                         FTP command list       
RSFTP007

*************************************************************************************
Converter campo float pra campo type p, decimal:

      CALL FUNCTION 'ROUND'
        EXPORTING
          DECIMALS            = 4
          INPUT               = fxvbapf-rmenge
          SIGN                = charx
       IMPORTING
          OUTPUT              = lf_rmenge
       EXCEPTIONS
          INPUT_INVALID       = 1
          OVERFLOW            = 2
          TYPE_INVALID        = 3
          OTHERS              = 4.           


  CALL FUNCTION 'SHP_ROUND'
    EXPORTING
      if_from       = if_float
    changing
      cf_to         = cf_packed.


Realizar conversão de valores (campo float):
  call function 'FLTP_CHAR_CONVERSION_FROM_SI'
    EXPORTING
      char_unit = ZEIEH "campo lido da T351P
      decimals = 0
      exponent = 0
      fltp_value_si = ZYKZT "campo lido da T351P
      indicator_value = 'X'
      masc_symbol = ' '
    IMPORTING
      char_value = char_wert. "campo a ser impresso

*************************************************************************************
Programa RSRLOGIN:
Login direto em outros ambientes.
Só entrar com 'Destino lógico' (na tela de seleção) e executar o programa

Tabela RFCDES (destino lógico p/função chamada remota)
*************************************************************************************
Bloqueiar execução de programa (evitar duplicidade de execução):

FORM bloquear_transacao .

  DATA: tl_enq_array TYPE STANDARD TABLE OF seqta,
        vl_enq_array TYPE seqta.

* Monta chave de bloqueio	
  CLEAR: vl_enq_array.
  vl_enq_array-gname = sy-repid.
  vl_enq_array-gmode = c_enqueue_mode.
* Monta Chave de bloqueio
  CONCATENATE sy-repid p_werks p_lgort INTO vl_enq_array-garg.
* Carrega tabela
  APPEND: vl_enq_array TO tl_enq_array.

* Bloqueia transação
  CALL FUNCTION 'ENQUEUE_ARRAY'
    EXPORTING
      synchron       = abap_true
    TABLES
      enq_array      = tl_enq_array
    EXCEPTIONS
      argument_error = 1
      foreign_lock   = 2
      own_lock       = 3
      system_failure = 4
      table_overflow = 5
      OTHERS         = 6.
* Verifica retorno
  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    STOP.
  ENDIF.

ENDFORM.

FORM desbloquear_transacao .

*Obs: O desbloqueio não se faz necessário. Qdo um report volta à tela de seleção, automaticamente o
*bloqueio se desfaz.

  DATA: tl_enq_array TYPE STANDARD TABLE OF seqta,
        vl_enq_array TYPE seqta.

* Monta chave de bloqueio
  CLEAR: vl_enq_array.
  vl_enq_array-gname = sy-repid.
  vl_enq_array-gmode = c_enqueue_mode.
* Monta Chave de bloqueio
  CONCATENATE sy-repid p_werks p_lgort INTO vl_enq_array-garg.
* Carrega tabela
  APPEND: vl_enq_array TO tl_enq_array.

* Bloqueia transação
  CALL FUNCTION 'DEQUEUE_ARRAY'
    EXPORTING
      synchron       = abap_true
    TABLES
      enq_array      = tl_enq_array
    EXCEPTIONS
      system_failure = 1
      OTHERS         = 2.

* Verifica retorno
  IF sy-subrc IS NOT INITIAL.
    MESSAGE ID sy-msgid TYPE 'I' NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.

*************************************************************************************
Programa gerado em tempo de execução:

FORM Z_ELIMINAR_PROJETO .

  data:       PROGRAM_NAME LIKE SY-CPROG .

  data: mess TYPE string.
  data: v_torr like e070a-trkorr.
  data: v_code(01).
  data: v_ok(01).
  data: t_fields like SVAL occurs 0 with header line.



  refresh source_table.

  " Grava os parametros para o Popup
  t_fields-tabname = 'E070A'.
  t_fields-FIELDNAME = 'TRKORR'.
  append t_fields.

  " Exibe o Popup para informar a request
  CALL FUNCTION 'POPUP_GET_VALUES_DB_CHECKED'
    EXPORTING
      POPUP_TITLE = 'Numero Request'
    IMPORTING
      RETURNCODE  = v_code
    TABLES
      FIELDS      = t_fields.

  read table t_fields index 1.
  check t_fields-value is not initial.

  " Move a request .
  move t_fields-value to v_torr.

  " Grava o codigo
  APPEND text-t01                      TO SOURCE_TABLE.
  APPEND text-t02                      TO SOURCE_TABLE.
  APPEND text-t03                      TO SOURCE_TABLE.
  APPEND text-t04                      TO SOURCE_TABLE.
  APPEND text-t05                      TO SOURCE_TABLE.
  APPEND text-t06                      TO SOURCE_TABLE.
  APPEND text-t07                      TO SOURCE_TABLE.
  APPEND text-t08                      TO SOURCE_TABLE.
  APPEND text-t09                      TO SOURCE_TABLE.
  APPEND text-t10                      TO SOURCE_TABLE.
  APPEND text-t11                      TO SOURCE_TABLE.
  APPEND text-t12                      TO SOURCE_TABLE.
  APPEND text-t13                      TO SOURCE_TABLE.
  APPEND text-t14                      TO SOURCE_TABLE.
  APPEND text-t15                      TO SOURCE_TABLE.
  APPEND text-t16                      TO SOURCE_TABLE.
  APPEND text-t17                      TO SOURCE_TABLE.
  APPEND text-t18                      TO SOURCE_TABLE.

* Gera o programa temporario
  clear mess.
  GENERATE SUBROUTINE POOL SOURCE_TABLE NAME PROGRAM_NAME
    MESSAGE mess .

  if mess is initial.
* Chama o programa
    PERFORM Z_ELIMINAR_CARGA IN PROGRAM (PROGRAM_NAME) using v_torr
                                                       CHANGING v_ok.
    if v_ok = ''.
      message i000(zsd) with text-e02.
    else.
      message i000(zsd) with text-e01.
    endif.
  else.
    message i000(zsd) with text-e01 .
  endif.
ENDFORM.                    " Z_ELIMINAR_PROJETO

T01	report zelimina.
T02	FORM Z_ELIMINAR_CARGA USING    P_TRKORR
T03	 CHANGING P_OK.
T04	tables: e070a.
T05	p_ok = 'X'.
T06	select single * from e070a
T07	where TRKORR = P_TRKORR
T08	and   ATTRIBUTE = 'SAP_CTS_PROJECT'.
T09	if sy-subrc eq 0.
T10	delete from e070a
T11	where TRKORR = P_TRKORR
T12	and   ATTRIBUTE = 'SAP_CTS_PROJECT'.
T13	if sy-subrc eq 0.
T14	commit work.
T15	clear p_ok.
T16	endif.
T17	endif.
T18	endform.
T19	report zelimina.
T20	FORM Z_MODIFICAR_PROJ USING    P_TRKORR  P_PROJ
T21	 CHANGING P_OK.
T22	tables: e070a.
T23	p_ok = 'X'.
T24	select single * from e070a
T25	where TRKORR = P_TRKORR
T26	and   ATTRIBUTE = 'SAP_CTS_PROJECT'.
T27	if sy-subrc eq 0.
T28	e070a-REFERENCE = P_PROJ.
T29	modify e070a .
T30	                                                                                                                                    
T31	if sy-subrc eq 0.
T32	commit work.
T33	clear p_ok.
T34	endif.
T35	endif.
T36	endform.
*************************************************************************************
Mostra tela com 'conditions' (vk11) (combinação de chaves):
  data: l_kozgf type kozgf.

  clear: t_tab_t, t_tab_t[].
  "Buscar sequência de acesso da condição
  select single kozgf into l_kozgf
    from t685
   where kvewe eq 'A'
     and kappl eq 'V'
     and kschl eq p_kschl.  "Condição da tela de seleção

  call function 'RV_GET_CONDITION_TABLES'
    exporting
      condition_use     = 'A'
      application       = 'V'
      condition_type    = p_kschl
      access_sequence   = l_kozgf
*      CONDITION_TABLE   = 'RV13A-KOTABNR
*      display_always    = display_always
*      table_check_rule  = table_check
      get_text          = 'X'
    importing
      table_t681        = t681
      table_tmc1t       = tmc1t
    exceptions
      no_selection_done = 4.

*************************************************************************************
CALL FUNCTION 'J_1B_FI_NETDUE'     "Calcula vencimento
         EXPORTING
              ZFBDT   =   BSID-ZFBDT
              ZBD1T   =   BSID-ZBD1T
              ZBD2T   =   BSID-ZBD2T
              ZBD3T   =   BSID-ZBD3T
         IMPORTING
              DUEDATE =   SEL-VENCI
         EXCEPTIONS
              OTHERS  = 1.

*************************************************************************************
Transação (se93) para chamar objeto webdynpro:
Na se93:
- Na aba 'Valor proposto para' entre com WDYID;
- Em 'vals.propostotes, entrar com:
  APPLICATION (em nome da área de tela), e nome do componente WD (em valor);
  STARTMODE (em nome da área de tela).

************
C14Z_MESSAGES_SHOW_AS_POPUP (log de mensagens retorno bapi):
Função que mostra um popup, que internamente faz todo o trabalho pra mostrar mensagens,
aquelas funções ('MESSAGES_INITIALIZE', 'MESSAGE_STORE', 'MESSAGES_STOP', 'MESSAGES_SHOW').
Passa como parâmetro uma tabela interna com as mensagens.

data: gt_return TYPE TABLE OF bapiret2,       "retorno da bapi
      ls_return  LIKE LINE OF gt_return,      "retorno da bapi
      lt_message TYPE TABLE OF y_message,
      ls_message LIKE LINE OF lt_message.

     LOOP AT gt_return INTO ls_return.
       ls_message-lineno = sy-tabix.
       ls_message-msgid  = ls_return-id.
       ls_message-msgty  = ls_return-type.
       ls_message-msgno  = ls_return-number.
       ls_message-msgv1  = ls_return-message_v1.
       ls_message-msgv2  = ls_return-message_v2.
       ls_message-msgv3  = ls_return-message_v3.
       ls_message-msgv4  = ls_return-message_v4.
       APPEND ls_message TO lt_message.
     ENDLOOP.

     CALL FUNCTION 'C14Z_MESSAGES_SHOW_AS_POPUP'
       TABLES
         i_message_tab = lt_message.

Popup para seleção de mês e ano (período):
  call function 'POPUP_TO_SELECT_MONTH'
    exporting
      actual_month               = vl_date(6)
      language                   = sy-langu
      start_column               = 8
      start_row                  = 5
    importing
      selected_month             = vl_month
      return_code                = vl_code
    exceptions
      factory_calendar_not_found = 1
      holiday_calendar_not_found = 2
      month_not_found            = 3
      others                     = 4.

  if vl_code is initial.
*    p_mesano = vl_month.
    concatenate vl_month(04) vl_month+4(02) into p_perio.
  endif.

*************************************************************************************************
Buscar tipo de campo numa função:

FUNCTION MATERIAL_UNIT_CONVERSION.
*"----------------------------------------------------------------------
*"*"Globale Schnittstelle:
*"       IMPORTING
*"             VALUE(INPUT) DEFAULT 0

data: T.

  "Verifica se é flutuante (F), decimal (P) ou inteiro (I).
  DESCRIBE FIELD INPUT TYPE T.
  IF T NE 'F' AND T NE 'P' AND T NE 'I'.
    MESSAGE E102 WITH 'INPUT' RAISING INPUT_INVALID.
  ENDIF.

*************************************************************************************************
             clear l_count.
             select count( * ) into l_count
                    from zcot003
                    where codsev = wa_zcot003-codsev.

*************************************************************************************************
Função que retorna exceções cadastrada para ST J1BTAX:
J_1B_READ_DYNAMIC_TABLE

************************************************************************************************* Início
data: it_fieldcat    type tfw_dfies_tab, "Tabela
      wa_fieldcat    like line of it_fieldcat.

  refresh it_fieldcat[].
  call function 'TFW_GET_DDIC_INFO'
    exporting
      iv_tabname            = 'NOME_ESTRUTURA_OU_TABELA'
    importing
      et_dfies              = it_fieldcat
    exceptions
      wrong_call            = 1
      erroneous_input_data  = 2
      ddic_data_read_failed = 3
      error_occurred        = 4
      others                = 5.

************************************************************************************************* Início
Deixar campos invisíveis em tela (pbo module-pool)
input = 1
invisible = 1
active = 0

************************************************************************************************* Início
Rotina pra evitar 'dump' (time out) com intervalo de datas muito grande, em tela de seleção:

at selection-screen.
  perform f_valida_periodos.

form f_valida_periodos.

  loop at screen.

    "Chama função que verifica se o campo da tela de seleção é do tipo
    "'select-options'. Se for, verifica se para o mesmo, existe um
    "registro na tabela ZFIT138, que define o nr de dias que podem ser
    "informados na tela de seleção.
    "Se o nr de dias informado pelo usuário na tela de seleção,
    "for maior que o nr de dias definido para o campo na tabela ZFIT138,
    "aborta o processamento do programa (emite mensagem de erro).
    call function 'ZDATA_SCREEN'
      exporting
        screen_name = screen-name.

  endloop.

endform.

function zdata_screen.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(SCREEN_NAME) TYPE  DDSHDYNFLD
*"----------------------------------------------------------------------

*-----------------------------------------------------------------------
*  Autor: Luismar A. C. Borges
*  Data : 24/03/2012
*  Objetivo:
*   "Verificar se o campo da tela (screen_name) de seleção é do tipo
*   "'select-options'. Se for, verificar se para o mesmo, existe um
*   "registro na tabela ZFIT138, que define o nr de dias que podem ser
*   "informados na tela de seleção.
*   "Se o nr de dias informado pelo usuário na tela de seleção, for maior
*   "que o nr de dias definido para o campo, na tabela ZFIT138, emite
*   "mensagem de erro.
*-----------------------------------------------------------------------

  data: v_campo        type zfit138-campo,
        v_ext(5)       type c,
        v_low(40)      type c,
        v_high(40)     type c,
        v_dias_char(9) type c,
        v_dias         type zfit138-nr_dias,
        v_dias_online  type zfit138-nr_dias,
        v_dias_backg   type zfit138-nr_dias_backg.

  field-symbols: <f_low>  type any,
                 <f_high> type any.

  check not screen_name is initial.
  check screen_name(1) ne '%'. "Descartar textos da tela...

  "Verifica se o campo da tela, é um 'select-options'...
  search screen_name for '-HIGH'.
  check sy-subrc = 0.

  "Extrai o nome técnico do campo...
  split screen_name at '-' into v_campo v_ext.

  clear: v_dias_online, v_dias_backg.

  "Verifica se o programa corrente e o nome técnico do campo da tela,
  "estão na tabela ZFIT138 (Período de data permitido em programas)...
  select single nr_dias nr_dias_backg
    into (v_dias_online, v_dias_backg)
    from zfit138
   where programa eq sy-cprog  "nome do programa
     and campo    eq v_campo.  "nome técnico do campo da tela

  check sy-subrc eq 0.

  "Monta os campos LOW e HIGH...
  concatenate '(' sy-cprog ')' v_campo '-LOW'  into v_low.
  concatenate '(' sy-cprog ')' v_campo '-HIGH' into v_high.

  assign (v_low)  to <f_low>.
  assign (v_high) to <f_high>.

  check ( <f_low> is assigned )
       and ( <f_high> is assigned ).

  "Verifica se valores inicial e final estão preenchidos...
  check ( <f_low> is not initial )
        and ( <f_high> is not initial ).

  "Se nr dias online está preenchido na tabela Z...
  if ( not v_dias_online is initial ) and
       ( sy-batch is initial ) "progr está rodando Online...
         and ( sy-ucomm ne 'SJOB' ).  "se não clicou no botão F9
    "F9 = background.
    "Verifica se qtde de dias informado na tela,
    "está dentro do nr dias permitido para execução...
    v_dias = <f_high> - <f_low>.
    if v_dias > v_dias_online.
      "retira os zeros à esquerda do nr de dias (tabela Z)...
      shift v_dias_online left deleting leading '0'.
      "retira os zeros à esquerda do nr de dias...
      shift v_dias left deleting leading '0'.
      concatenate '(' v_dias ')' into v_dias_char.
      message e034(zmsg) with v_dias_char 'Online' v_dias_online.
    endif.
  endif.

  "Se nr dias em background está preenchido na tabela Z...
  if ( not v_dias_backg is initial ) and
   "se clicou no botão F9 ou o progr está rodando em background...
      ( sy-ucomm = 'SJOB' or sy-batch = 'X' ).
    "Verifica se qtde de dias informado na tela,
    "está dentro do nr dias permitido para execução...
    v_dias = <f_high> - <f_low>.
    if v_dias > v_dias_backg.
      "retira os zeros à esquerda do nr de dias (tabela Z)...
      shift v_dias_backg left deleting leading '0'.
      "retira os zeros à esquerda do nr de dias...
      shift v_dias left deleting leading '0'.
      concatenate '(' v_dias ')' into v_dias_char.
      message e034(zmsg) with v_dias_char 'Background' v_dias_backg.
    endif.
  endif.

endfunction.

e034(zmsg) "Nr de dias & não permitido para execução &! Nr dias permitido: &"

-------
Tabela transp.     ZFIT138
Descrição breve    Período de datas permitido em execuções de programas

MANDT   	MANDT	CLNT	3	0	Mandante
PROGRAMA	REPID	CHAR	40	0	Nome do programa ABAP
CAMPO	        FIELD	CHAR	10	0	Nome do campo
NR_DIAS		        NUMC	5	0	Nr dias permitido para execução (on line)
NR_DIAS_BACKG		NUMC	5	0	Nr dias permitido para execução (em BACKGROUND)

-------

************************************************************************************************* Fim
O programa não pode ser executado em background:
at selection-screen.
   perform f_valida_execucao.

form f_valida_execucao.

   "O programa não pode ser executado em background
  if ( sy-ucomm = 'SJOB' or sy-batch = 'X' ).
    message e000 with 'Este programa não pode ser executado em background!'.
  endif.

endform.

******************************************************************
Programa para criação de projetos (controle em requests): RSWBO_AUX_PROJECT

******************************************************************
PROCESS  ON  VALUE-REQUEST.
         vbap-zz_motivo MODULE create_dropdown_motivo.

MODULE create_dropdown_motivo INPUT.

  DATA: BEGIN OF it_motivo OCCURS 0,
          item  TYPE ztmm_motivos-item,
          marca TYPE ztmm_motivos-marca,
        END OF it_motivo.

  SELECT item marca
    INTO TABLE it_motivo
    FROM ztmm_motivos.

  IF sy-subrc IS INITIAL.

    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
        retfield        = 'ITEM'
        value_org       = 'S'
      TABLES
        value_tab       = it_motivo
      EXCEPTIONS
        parameter_error = 1
        no_values_found = 2
        OTHERS          = 3.
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDIF.

ENDMODULE.

******************************************************************-------------------------
Evitar DUMP, quando entrada excessiva de valores em tela de seleção...

    try.
        select * from ztge_constantes into table t_dados where (v_where).
      catch cx_sy_open_sql_db. "SS 277259...
        "Este Try e Catch evita dump, caso usuário entrar com qtde
        "excessiva de valores na tela seleção.
        message e033(zmsg).
    endtry.

******************************************************************-------------------------
Programa RSAQSHSG (tela customer de modelo diferente)
******************************************************************-------------------------

       SEARCH screen-name FOR '-LOW'.

******************************************************************-------------------------
Alterar data validade/vencimento lote:

    clear wa_mch1.

    select single *
      from mch1
      into wa_mch1
     where charg eq t_fabric-lotpi
     and   matnr eq t_fabric-matpi.

    if wa_mch1-hsdat is initial.

      write: /01 sy-vline,
              06 t_fabric-matnr,
              20 t_fabric-charg,
              40 t_fabric-lotpi,
              60 'Lote de PI com data de validade em branco!',
             150 sy-vline.

    else.

      e_batch-prod_date = wa_mch1-hsdat.
      e_batchx-prod_date = 'X'.

*       Calcula data de validade
      select single mhdhb
        from mara
        into vd_meses
       where matnr eq t_fabric-matnr.

      vc_meses = vd_meses.

      call function 'RP_CALC_DATE_IN_INTERVAL'
        exporting
          date      = e_batch-prod_date
          days      = '00'
          months    = vc_meses
          signum    = '+'
          years     = '00'
        importing
          calc_date = e_batch-expirydate.

      e_batchx-expirydate = 'X'.

      call function 'BAPI_BATCH_CHANGE'
        exporting
          material         = t_fabric-matnr
          batch            = t_fabric-charg
          batchattributes  = e_batch
          batchattributesx = e_batchx
        tables
          return           = t_return.

      sort t_return by type.
      read table t_return with key type = 'E'
                        binary search.
      if sy-subrc is initial.
        write: /01 sy-vline,
                06 t_fabric-matnr,
                20 t_fabric-charg,
                40 t_fabric-lotpi,
                60 'Erro na Função BAPI_BATCH_CHANGE', t_return-id, t_return-number,
               150 sy-vline.
        rollback work.
      else.
        write: /01 sy-vline,
                06 t_fabric-matnr,
                20 t_fabric-charg,
                40 t_fabric-lotpi,
                60 wa_mch1-hsdat,
               150 sy-vline.
        update ztpp_fabric_pi
           set dtatr = sy-datum
               hratr = sy-uzeit
               produ = e_batch-prod_date
               valid = e_batch-expirydate
         where matnr eq t_fabric-matnr
           and charg eq t_fabric-charg
           and lotpi eq t_fabric-lotpi
           and dtatr eq '00000000'.
        commit work.
      endif.
    endif.

******************************************************************-------------------------
Buscar segunda linguagem:

data: second_language   type syst-langu.

  call 'C_SAPGPARAM' id 'NAME'  field 'zcsa/second_language'
                     id 'VALUE' field second_language.

  "O exemplo abaixo busca dados pela linguagem de login.
  "Caso não encontre, busca por uma segunda linguagem.

  select * from snapt where langu = sy-langu
                      and   ttype = ttype
                      and   errid = errid    order by primary key.

    text_in-line = snapt-tline.  append text_in.
  endselect.

  if sy-subrc <> 0.
    select * from snapt where langu = second_language
                        and   ttype = ttype
                        and   errid = errid    order by primary key.

      text_in-line = snapt-tline.  append text_in.
    endselect.
  endif.

******************************************************************-------------------------
Caracteres ASC:
  CALL FUNCTION 'FI_DME_CHARACTERS'
    IMPORTING
      e_cr  = cr
      e_lf  = lf
      e_eof = eof.

SCP_REPLACE_STRANGE_CHARS

******************************************************************-------------------------
Bloquear usuário:
  CALL FUNCTION 'SUSR_USER_LOCK'
       EXPORTING
            USER_NAME           = USER_NAME
            LOCK_GLOBAL         = LOCK_GLOBAL
            LOCK_LOCAL          = LOCK_LOCAL
       EXCEPTIONS
            USER_NAME_NOT_EXIST = 1
            OTHERS              = 2.

Desbloquear usuário:
  CALL FUNCTION 'SUSR_USER_UNLOCK'
       EXPORTING
            USER_NAME           = USER_NAME
            LOCK_GLOBAL         = LOCK_GLOBAL
            LOCK_LOCAL          = LOCK_LOCAL
            LOCK_WRONG_LOGON    = LOCK_WRONG_LOGON
       EXCEPTIONS
            USER_NAME_NOT_EXIST = 1
            OTHERS              = 2.

******************************************************************-------------------------
Buscar valores de campos em uma tela de seleção de um programa:

    data:   lc_msg      type char100,
            v_field     TYPE dynpread-fieldname VALUE 'RL01S-LGORT',
            v_cprog     TYPE sy-cprog VALUE 'SAPML01S',
            v_dynnr_209 TYPE sy-dynnr VALUE '0209',
            v_dynnr_211 TYPE sy-dynnr VALUE '0211',
            v_lgort     TYPE mseg-lgort.

    "Luismar (22/12/2011) SS 254142 - Início
    "Busca o valor do campo 'depósito' na transação LS24
    CLEAR v_lgort.
    CALL FUNCTION 'GET_DYNP_VALUE'
      EXPORTING
        i_field = v_field
        i_repid = v_cprog
        i_dynnr = v_dynnr_209
      CHANGING
        o_value = v_lgort.

******************************************************************-------------------------
Download de arquivo: online ou background.

selection-screen begin of block b1 with frame title text-002.
parameters: p_pc      radiobutton group 0001 default 'X',
            p_sr      radiobutton group 0001,
            p_arq     type zhrddire obligatory.
selection-screen end of block b1.

data: v_arquivo   type string,
      v_erro(130) type c.

at selection-screen on value-request for p_arq.
* Abre caixa de diálogo para seleção do diretório do arquivo saida
  perform f_caminho_arq changing p_arq.

  v_arquivo = p_arq.

  if p_pc eq 'X'.

    call function 'GUI_DOWNLOAD'
      exporting
        filename                = v_arquivo
      tables
        data_tab                = t_cargos
      exceptions
        file_write_error        = 1
        no_batch                = 2
        gui_refuse_filetransfer = 3
        invalid_type            = 4
        no_authority            = 5
        unknown_error           = 6
        header_not_allowed      = 7
        separator_not_allowed   = 8
        filesize_not_allowed    = 9
        header_too_long         = 10
        dp_error_create         = 11
        dp_error_send           = 12
        dp_error_write          = 13
        unknown_dp_error        = 14
        access_denied           = 15
        dp_out_of_memory        = 16
        disk_full               = 17
        dp_timeout              = 18
        file_not_found          = 19
        dataprovider_exception  = 20
        control_flush_error     = 21
        others                  = 22.

    if sy-subrc = 0.
      message i000 with 'Arquivo: ' v_arquivo 'gerado com sucesso!'.
    else.
      message e000 with 'Erro na gravação do arquivo.'
                        'Codigo de retorno' sy-subrc.
    endif.
  else.

    open dataset v_arquivo
     for output in text mode encoding default message v_erro.

    if sy-subrc eq 0.

      loop at t_cargos.
        transfer t_cargos to v_arquivo.
      endloop.

      write: /01 'Arquivo:', v_arquivo, 'gerado com sucesso!'.

    else.
      message e000 with v_erro.
    endif.

    close dataset v_arquivo.

  endif.

form f_caminho_arq changing p_arq.

  data: l_mask type string value 'Arq texto|*.TXT'.

  DATA: l_filename   TYPE string,
        t_file_table TYPE filetable,
        l_title      TYPE string,
        l_rc         TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = 'Seleção do diretório e arquivo'
      default_filename        = l_filename
      default_extension       = l_mask
      file_filter             = l_mask
    CHANGING
      file_table              = t_file_table
      rc                      = l_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  CHECK sy-subrc IS INITIAL.

  READ TABLE t_file_table INTO p_arq INDEX 1.

ENDFORM.


--------------
* Abre diretórios(PASTA) (somente diretórios)

at selection-screen on value-request for p_file.
  perform f4_diretorio.

form f4_diretorio .

  data:
    l_path type string,
    l_len(3) type n.

  if p_servi is initial.
    call method cl_gui_frontend_services=>directory_browse
      exporting
        window_title    = 'Selecione um diretório'
        initial_folder  = 'C:\'
      changing
        selected_folder = l_path.
    call method cl_gui_cfw=>flush.

    l_len = strlen( l_path ) - 1.

    if not l_path is initial.
      if l_path+l_len(1) ne '\'.
        concatenate l_path '\' into p_file.
      else.
        p_file = l_path.
      endif.
    endif.
  endif.

******************************************************************-------------------------
SE16N
Para Editar, modificar (em tempo de debug) "X" os campos:
GD-EDIT
GD-SAPEDIT

-------------------------------------------------------
Transação SCU3
Tabela SE16N_CD_DATA (guarda doc modificação de dados)
-------------------------------------------------------
Técnicas de debug:

http://www.sdn.sap.com/irj/scn/weblogs?blog=/pub/wlg/18861
-------------------------------------------------------

* converter data

      CALL FUNCTION 'CONVERT_DATE_TO_INTERNAL'
           EXPORTING
                DATE_EXTERNAL   = T_HEADER-DATGER     “ex.:14042004 ou 14.04.2004
           IMPORTING
                DATE_INTERNAL   = WS_DATA             “sy-datum  --- 20040414
           EXCEPTIONS
                DATE_EXTERNAL_IS_INVALID = 1
                OTHERS                   = 2.

CONVERT DATE s_dt_lib INTO INVERTED-DATE v_mesano_aux
                               20050722 (type sy-datum)                 79949298 (char 8)
     CONVERT INVERTED-DATE wa_tcurr_aux-gdatu INTO DATE wa_tcurr-data.
                                              79949298 (char 8)                            20050722 (type sy-datum)

-------------------------------------------------------

*-----------------
"Estruturas para função que modifica documento contábil...
DATA: BEGIN OF t_bkpf OCCURS 02.
        INCLUDE STRUCTURE bkpf.
DATA: END OF t_bkpf.

DATA: BEGIN OF t_bkdf OCCURS 02.
        INCLUDE STRUCTURE bkdf.
DATA: END OF t_bkdf.

DATA: BEGIN OF t_bseg OCCURS 02.
        INCLUDE STRUCTURE bseg.
DATA: END OF t_bseg.

DATA: BEGIN OF t_bsec OCCURS 02.
        INCLUDE STRUCTURE bsec.
DATA: END OF t_bsec.

DATA: BEGIN OF t_bsed OCCURS 02.
        INCLUDE STRUCTURE bsed.
DATA: END OF t_bsed.

DATA: BEGIN OF t_bset OCCURS 02.
        INCLUDE STRUCTURE bset.
DATA: END OF t_bset.

* table with the NEW content of: BSEC
DATA: BEGIN OF xbsec OCCURS 20.
        INCLUDE STRUCTURE fbsec.
DATA: END OF xbsec.

* table with the OLD content of: BSEC
DATA: BEGIN OF ybsec OCCURS 20.
        INCLUDE STRUCTURE fbsec.
DATA: END OF ybsec.

* table with the NEW content of: BSED
DATA: BEGIN OF xbsed OCCURS 20.
        INCLUDE STRUCTURE fbsed.
DATA: END OF xbsed.

* table with the OLD content of: BSED
DATA: BEGIN OF ybsed OCCURS 20.
        INCLUDE STRUCTURE fbsed.
DATA: END OF ybsed.

* table with the NEW content of: BSEG
DATA: BEGIN OF xbseg OCCURS 20.
        INCLUDE STRUCTURE fbseg.
DATA: END OF xbseg.

* table with the OLD content of: BSEG
DATA: BEGIN OF ybseg OCCURS 20.
        INCLUDE STRUCTURE fbseg.
DATA: END OF ybseg.

* table with the NEW content of: BSEG_ADD
DATA: BEGIN OF xbseg_add OCCURS 20.
        INCLUDE STRUCTURE fbseg_add.
DATA: END OF xbseg_add.

* table with the OLD content of: BSEG_ADD
DATA: BEGIN OF ybseg_add OCCURS 20.
        INCLUDE STRUCTURE fbseg_add.
DATA: END OF ybseg_add.

* table with the NEW content of: BSET
DATA: BEGIN OF xbset OCCURS 20.
        INCLUDE STRUCTURE fbset.
DATA: END OF xbset.

* table with the OLD content of: BSET
DATA: BEGIN OF ybset OCCURS 20.
        INCLUDE STRUCTURE fbset.
DATA: END OF ybset.
*-----------------

FORM f_proc_fi_document_change.
  DATA: BEGIN OF lt_bseg OCCURS 0.
          INCLUDE STRUCTURE bseg.
  DATA: END OF lt_bseg.

  DATA: lt_bsid    TYPE TABLE OF bsid,
        lw_bsid    TYPE bsid.

  vg_subrc_bseg = 4.

  "Prepara estruturas (tabelas internas) para modificação do item contábil...
  REFRESH: t_bkpf[], t_bseg[].

  "Dados cabeçalho...
  SELECT * FROM bkpf INTO TABLE t_bkpf
         WHERE bukrs = eg_alv-bukrs
           AND belnr = eg_alv-belnr
           AND gjahr = eg_alv-gjahr.

  CHECK sy-subrc = 0.

  "Dados item...
  SELECT * FROM bseg INTO TABLE t_bseg
         WHERE bukrs = eg_alv-bukrs
           AND belnr = eg_alv-belnr
           AND gjahr = eg_alv-gjahr
           AND buzei = eg_alv-buzei.

  CHECK sy-subrc = 0.

  REFRESH: xbseg[], ybseg[].

  "Move valores a serem alterados no item contábil (tabela interna itens)...
  LOOP AT t_bseg.
    CLEAR: xbseg, ybseg.

    "Move dados (old) para tabela interna...
    MOVE-CORRESPONDING t_bseg TO ybseg.
    APPEND ybseg.

    t_bseg-zfbdt = eg_alv-novo_vencto. "Muda para novo vencimento

    "Se não estiver em carteira...
    IF t_bseg-hbkid NE 'CART'.
      t_bseg-dtws1 = '6'.   "Alterar valor campo Instrução 1
      t_bseg-zlsch = 'A'.   "Alterar valor campo Forma de pagamento
    ENDIF.

    CLEAR : t_bseg-zbd1t, "Limpar o campo 'Dias de desconto 1'
            t_bseg-anfbn, "Limpar o campo 'Nº documento da solicitação de letra de câmbio'
            t_bseg-anfbu, "Limpar o campo 'Empresa, na qual foi registrada a solicitação de L/C'
            t_bseg-anfbj. "Limpar o campo 'Exercício do documento de solicitação de letra de câmbio'

    t_bseg-dtws4 = '99'.    "Indica que foi atualizado no novo sistema

    MODIFY t_bseg INDEX sy-tabix.

    "Move dados (new) para tabela interna...
    MOVE-CORRESPONDING t_bseg TO xbseg.
    APPEND xbseg.
  ENDLOOP.

*----------------------------------------------------------------------
  "Rotina para pesquisar os dados dos créditos com fatura relacionada
  SELECT * INTO TABLE lt_bsid
    FROM bsid
    WHERE bukrs = eg_alv-bukrs
      AND rebzg = eg_alv-belnr
      AND rebzj = eg_alv-gjahr
      AND rebzz = eg_alv-buzei
      AND shkzg = c_h
      AND ( zlspr NE  c_s AND zlspr NE  c_o AND zlspr NE space ).
  IF sy-subrc EQ 0.
    FREE lt_bseg.
    SORT lt_bsid BY bukrs gjahr belnr buzei.

    SELECT * FROM bseg INTO TABLE lt_bseg
      FOR ALL ENTRIES IN lt_bsid
           WHERE bukrs = lt_bsid-bukrs
             AND belnr = lt_bsid-belnr
             AND gjahr = lt_bsid-gjahr
             AND buzei = lt_bsid-buzei.
    IF sy-subrc EQ 0.
      "Dados cabeçalho dos créditos relacionados...
      SELECT * FROM bkpf APPENDING TABLE t_bkpf
        FOR ALL ENTRIES IN lt_bseg
             WHERE bukrs = lt_bseg-bukrs
               AND belnr = lt_bseg-belnr
               AND gjahr = lt_bseg-gjahr.

    ENDIF.
    LOOP AT lt_bseg .

      READ TABLE lt_bsid INTO lw_bsid
        WITH KEY bukrs = lt_bseg-bukrs
                 gjahr = lt_bseg-gjahr
                 belnr = lt_bseg-belnr
                 buzei = lt_bseg-buzei BINARY SEARCH.
      IF sy-subrc NE 0.
        CONTINUE.
      ENDIF.

      CLEAR: xbseg, ybseg, t_bseg.

      "Move dados (old) para tabela interna...
      MOVE-CORRESPONDING lt_bseg TO t_bseg.
      MOVE-CORRESPONDING t_bseg TO ybseg.
      APPEND ybseg.

      t_bseg-zfbdt = eg_alv-novo_vencto. "Nova data base vencimento
      t_bseg-hbkid = lw_bsid-hbkid.         "Banco empresa = do título
      t_bseg-dtws1 = lw_bsid-dtws1.         "Instrução 1   = do título

      CLEAR : t_bseg-zbd1t, "Dias de desconto 1
              t_bseg-anfbn, "Nº doc. solicitação de letra de câmbio
              t_bseg-anfbu, "Empresa, da solicitação de L/C'
              t_bseg-anfbj. "Exercício da solicitação de letra de câmbio

      "Se o título não estiver em carteira...
      IF t_bseg-hbkid NE c_cart.
        t_bseg-dtws1 = c_6.   "Alterar valor campo Instrução 1
        t_bseg-zlsch = c_a.   "Alterar valor campo Forma de pagamento
      ENDIF.
      APPEND t_bseg.

      "Move dados (new) para tabela interna...
      MOVE-CORRESPONDING t_bseg TO xbseg.
      APPEND xbseg.
    ENDLOOP.
  ENDIF.
*-----------------------------------------------------------------------

  "Altera registro documento contábil...
  CALL FUNCTION 'CHANGE_DOCUMENT'
    TABLES
      t_bkpf = t_bkpf
      t_bkdf = t_bkdf
      t_bseg = t_bseg
      t_bsec = t_bsec
      t_bsed = t_bsed
      t_bset = t_bset.

  vg_subrc_bseg = sy-subrc.

  IF sy-subrc NE 0.
    "Erro ao modificar documento contábil:
    CONCATENATE text-m01 eg_alv-bukrs eg_alv-belnr eg_alv-gjahr INTO vg_text.
  ELSE.
    PERFORM f_log_modificacao.
    "Documento contábil: & modificado com sucesso
    CONCATENATE text-m02 eg_alv-bukrs eg_alv-belnr eg_alv-gjahr text-m03 INTO vg_text.
  ENDIF.

  PERFORM f_grava_log_processos USING vg_text.

ENDFORM.                    "f_proc_fi_document_change


FORM f_log_modificacao.
  DATA: l_objectid                TYPE cdhdr-objectid,
        l_tcode                   TYPE cdhdr-tcode,
        l_planned_change_number   TYPE cdhdr-planchngnr,
        l_utime                   TYPE cdhdr-utime,
        l_upd_bkdf                TYPE c,
        l_upd_bsec                TYPE c,
        l_upd_bkpf                TYPE c,
        l_upd_bsed                TYPE c,
        l_upd_bset                TYPE c,
        l_upd_bseg                TYPE c VALUE 'U',
        l_upd_bseg_add            TYPE c VALUE 'U',
        l_udate                   TYPE cdhdr-udate,
        l_username                TYPE cdhdr-username,
        l_cdoc_planned_or_real    TYPE cdhdr-change_ind,
        l_cdoc_upd_object         TYPE cdhdr-change_ind VALUE 'U',
        l_cdoc_no_change_pointers TYPE cdhdr-change_ind.

  CLEAR l_objectid.
  CONCATENATE sy-mandt eg_alv-bukrs eg_alv-belnr eg_alv-gjahr INTO l_objectid.
  l_utime    = sy-uzeit.
  l_udate    = sy-datum.
  l_username = sy-uname.
  l_tcode    = sy-tcode.

  CALL FUNCTION 'BELEG_WRITE_DOCUMENT'
    EXPORTING
      objectid                = l_objectid
      tcode                   = l_tcode
      utime                   = l_utime
      udate                   = l_udate
      username                = l_username
      planned_change_number   = l_planned_change_number
      object_change_indicator = l_cdoc_upd_object
      planned_or_real_changes = l_cdoc_planned_or_real
      no_change_pointers      = l_cdoc_no_change_pointers
      o_bkdf                  = *bkdf
      n_bkdf                  = bkdf
      upd_bkdf                = l_upd_bkdf
      o_bkpf                  = *bkpf
      n_bkpf                  = bkpf
      upd_bkpf                = l_upd_bkpf
      upd_bsec                = l_upd_bsec
      upd_bsed                = l_upd_bsed
      upd_bseg                = l_upd_bseg
      upd_bseg_add            = l_upd_bseg_add
      upd_bset                = l_upd_bset
    TABLES
      xbsec                   = xbsec
      ybsec                   = ybsec
      xbsed                   = xbsed
      ybsed                   = ybsed
      xbseg                   = xbseg
      ybseg                   = ybseg
      xbseg_add               = xbseg_add
      ybseg_add               = ybseg_add
      xbset                   = xbset
      ybset                   = ybset.

ENDFORM.                    "f_log_modificacao
-------------------------------------------------------
Atualizar tabela log modificação por função:
'VERKBELEG_WRITE_DOCUMENT'  (exemplo da VA02)
    |
    |
   objeto

-------------------------------------------------------
Opção 1 - Constantes Standard SAP
Opção 2 - Características Standard SAP
Opção 3 - Sets Standard SAP

Opção 1:
Transação: STVARV	Manutenção de Variáveis   (tabela TVARVC)

Transação: GS01
A tabela a ser lida é a SETLEAF.

data: r_rufnm type range of pa0002-rufnm.

     select valsign valoption valfrom valto
            into table r_rufnm
        from setleaf
             where setname eq 'ZFI_SUPERAPROV'.

ou:

Função que retorna tabela com valores do set:
  data: lt_values  like rgsb4 occurs 0 with header line,
  ranges: r_chbloq  for bsik-ZLSPR.

      REFRESH lt_values.
      CALL FUNCTION 'G_SET_GET_ALL_VALUES'
        EXPORTING
          setnr         = 'ZFI_TRIBUTOS'
          class         = '0000'
        TABLES
          set_values    = lt_values
        EXCEPTIONS
          set_not_found = 1
          others        = 2.

      IF sy-subrc IS INITIAL.
        LOOP AT lt_values.
          r_chbloq-sign = 'I'.
          r_chbloq-option = 'EQ'.
          r_chbloq-low = lt_values-from.
          APPEND r_chbloq.
        endloop.
      ENDIF.

-------------------------------------------------------
Função TFW_GET_DDIC_INFO ou DDIF_NAMETAB_GET:
Retorna nome dos campos de uma tabela
-------------------------------------------------------

J_1B_SD_SA_CHANGE_VBAP

-------------------------------------------------------
Zeros à esquerda:
      perform f_zeros_esquerda changing t_excel-cpf.

form f_zeros_esquerda changing p_cpf.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
         EXPORTING
              INPUT  = p_cpf
         IMPORTING
              OUTPUT = p_cpf(11)
         EXCEPTIONS
              OTHERS = 1.

endform.

-------------------------------------------------------
"Troca todos os pontos (.) por espaço:
replace all occurrences  of '.' in v_val1 with ' '.


REPLACE ALL OCCURRENCES OF REGEX '[^[:alnum:]]' IN lv_text WITH ''.

   "Substitui o ponto de milhões por espaço...
    replace '.' with ' ' into p_valor.
    condense p_valor NO-GAPS.

   "Substitui o ponto de milhares por espaço...
    replace '.' with ' ' into p_valor.
    condense p_valor NO-GAPS.

   "Substitui vírgula (,) por ponto (.)...
    replace ',' with '.' into p_valor.
    condense p_valor NO-GAPS.
-------------------------------------------------------


segue um exemplo de como usar Field Symbol em colunas de tabela interna.
 
TYPES:
 
BEGIN OF ty_op_cuenta,
        bukrs TYPE glt0-bukrs, "Sociedad
        racct TYPE glt0-racct, "Cuenta
        ryear TYPE glt0-ryear, "Ejercicio
        hsl01 TYPE glt0-hsl01, "Total de movimientos del período en moneda local
        hsl02 TYPE glt0-hsl02, "Total de movimientos del período en moneda local
        hsl03 TYPE glt0-hsl03, "Total de movimientos del período en moneda local
        hsl04 TYPE glt0-hsl04, "Total de movimientos del período en moneda local
        hsl05 TYPE glt0-hsl05, "Total de movimientos del período en moneda local
        hsl06 TYPE glt0-hsl06, "Total de movimientos del período en moneda local
        hsl07 TYPE glt0-hsl07, "Total de movimientos del período en moneda local
        hsl08 TYPE glt0-hsl08, "Total de movimientos del período en moneda local
        hsl09 TYPE glt0-hsl09, "Total de movimientos del período en moneda local
        hsl10 TYPE glt0-hsl10, "Total de movimientos del período en moneda local
        hsl11 TYPE glt0-hsl11, "Total de movimientos del período en moneda local
        hsl12 TYPE glt0-hsl12, "Total de movimientos del período en moneda local
        hsl13 TYPE glt0-hsl13, "Total de movimientos del período en moneda local
        hsl14 TYPE glt0-hsl14, "Total de movimientos del período en moneda local
        hsl15 TYPE glt0-hsl15, "Total de movimientos del período en moneda local
        hsl16 TYPE glt0-hsl16, "Total de movimientos del período en moneda local
        total TYPE glt0-hsl01, "Total de movimientos.
      END OF ty_op_cuenta.

 
DATA:  t_op_cuenta TYPE STANDARD TABLE OF ty_op_cuenta,
             l_perio(2)  TYPE n,
             l_valor(19) TYPE c,
             val_per     TYPE glt0-hsl01.

 
  FIELD-SYMBOLS:  <valor> TYPE glt0-hsl01,
                                  <f_op_cuenta> TYPE ty_op_cuenta.

  LOOP AT t_op_cuenta ASSIGNING <f_op_cuenta>.

      DO 16 TIMES.
        l_perio = sy-index.
        CONCATENATE '<f_op_cuenta>-'
                    'hsl'
                    l_perio
               INTO l_valor.
        ASSIGN (l_valor) TO <valor>.
        MOVE <valor> TO val_per.
      ENDDO.

  ENDLOOP.

-------------------------------------------------------
campo dinâmico na tela de seleção...
SELECT-OPTIONS: files FOR ('HEADA-ARKEY').

-------------------------------------------------------
Assign campo select-option de um programa qualquer.

    field-symbols <f_bukrs> type any table.
    "Busca empresa(s) inseridas na tela de seleção
    assign ('(HBRCCED0)S_BUKRS[]') to <f_bukrs>.
    if <f_bukrs> is assigned.

-------------------------------------------------------
"Força a alteração de uma ordem de venda.

    field-symbols <f_DATALOSS> type R185D-DATALOSS.

              ASSIGN ('(SAPLJ_1B_NFE)R185D-DATALOSS') TO <f_dataloss>.
              if <f_dataloss> is ASSIGNED.
                 <f_dataloss> = 'X'.
              endif.

Obs: Move 'X' para campo que checa se houve modificação de algum campo.
SAPMV45A                       / MV45AF0B_BELEG_SICHERN
FORM                           / BELEG_SICHERN
    if r185d-dataloss = space     and
       force_update   = space     and
       upd_vbuk is initial and
       upd_vbup is initial.

-------------------------------------------------------
Para encontrar a 'Operação' (na Miro):

  field-symbols: <fs_op> type any.
    assign ('(SAPLMR1M)RM08M-VORGANG') to <fs_op>.
    if <fs_op> is assigned.

------ ou
Buscar os campos 'xrech' (tabela  RBKP) e 'tbtkz' (tabela RBKPB).

Consistir os campos:
  IF rbkpv-xrech = space.
    IF rbkpv-tbtkz = space.
      rm08m-vorgang = '2'.             "Nota de crédito
    ELSE.
      rm08m-vorgang = '4'.             "Crédito posterior
    ENDIF.
  ELSE.
    IF rbkpv-tbtkz = space.
      rm08m-vorgang = '1'.             "Fatura
    ELSE.
      rm08m-vorgang = '3'.             "Débito posterior
    ENDIF.
  ENDIF.

-------------------------------------------------------
Tipo de dados    LRAW
display_list  (mostra lista em modo ASCI)
LIST_TO_ASCI   (converte tabela p/ tabela ASCI)
-------------------------------------------------------

172

LJ_1B_NFEF42
J_1B_NFE_XML_OUT

SAPLRWCL                       / LRWCLF01
FORM                           / DOCUMENT_POST
XACCTIT


SAPLRWCL                       / LRWCLU02
FORM                           / DOCUMENT_MM_POST



SAPLFACI                       / LFACIF52
FORM                           / UPDATE_WT


LJ1BBU14
LJ_1B_NFEU26

LJ1BFU02
PERFORM post_nfimfi

MIGO:
SAPMM07M                       / MM07MFJ1
FORM                           / J_1B_NF_DOCUMENT_SAVE
----xmkpf xaccit      mover valores aqui
4900235877
4900000014


SAPLJ_1B_NFE                   / LJ_1B_NFEU26
FUNCTION                       / J_1B_NFE_OUT_OK
buscou nr nfe  --- moveu para CS_HEADER-NFENUM.

SAPLJ1BB                       / LJ1BBU14                       /
FUNCTION                       / J_1B_NF_DOC_INSERT_FROM_OBJECT
WK_HEADER-NFENUM


SAPLJ1BB                       / LJ1BBU14
FORM                           / ARCHIVE_LINK_BARCODE_UPDATE
aqui talvez a gente exporte pra memória o nfenum (WK_HEADER-NFENUM)

SAPLJ1BF                       / LJ1BFF06
FORM                           / POST_NFIMFI
Aqui alterar o XLBLNR.

SAPLMBWL                       / LMBWLU21
FUNCTION                       / MB_POST_GOODS_MOVEMENT


Onde tem belnr preenchido
SAPLFACI                       / LFACIF5D
FORM                           / FI_DOCUMENT_PREPARE

-----
FV60 (Pré-editar fatura de fornecedor):
No momento da salvar documento: bTE SAMPLE_INTERFACE_00002218

FB01...FB0?, FB08, etc.

I_AKTYP	= 'A' --- variável indicando exibição
I_AKTYP	= 'V' --- variável indicando modificação

Bte SAMPLE_INTERFACE_00001030 (na criação (fb01) do docto contábil, ou estorno (fb08) após obter nr do docto contábil)
Bte SAMPLE_INTERFACE_00001110 (na modificação (fb01) do docto contábil, após clicar botão salvar)

Momento gravação das tabelas de contabilidade BKPF, BSEG  e cia.
SAPLFACI                       / LFACIF57	
FORM                           / FI_DOCUMENT_POST  (chama a função POST_DOCUMENT)
(SAPLFACI)XBKPF
  IF POST_MODE IS INITIAL.
    CALL FUNCTION 'POST_DOCUMENT' IN UPDATE TASK
         TABLES
           ...
ou
include MF05AFF0_FCODE_BEARBEITUNG (ENHANCEMENT-POINT mf05aff0_fcode_bearbeitung_01 SPOTS es_sapmf05a)

-----------

SAPMV50A                       / FV50XF0B_BELEG_SICHERN
FORM                           / BELEG_SICHERN_POST
commit




LV_NR_RANGE	                                   	55
LV_OBJECT	                                   	J_1BNFENUM

4900235867


***************************
Retorna valor NF:
J_1B_NF_VALUE_DETERMINATION (todos os itens da NF)

J_1B_NF_VALUE_DETERMINATION_I (valor por item nf)
Abaixo, estruturas para busca por item:

      begin of type_j_1bnflin,
          mandt    type j_1bnflin-mandt,
          docnum   type j_1bnflin-docnum,
          itmnum   type j_1bnflin-itmnum,
          refkey   type j_1bnflin-refkey,
          menge    type j_1bnflin-menge ,
          meins    type j_1bnflin-meins ,
          netpr    type j_1bnflin-netpr ,
          netwr    type j_1bnflin-netwr ,
          tmiss    type j_1bnflin-tmiss ,
          netfre   type j_1bnflin-netfre,
          netins   type j_1bnflin-netins,
          netoth   type j_1bnflin-netoth,
          netdis   type j_1bnflin-netdis,
          incltx   type j_1bnflin-incltx,
          kunnr_re type vbpa-kunnr,
          kunnr_tr type vbpa-kunnr,
      end of type_j_1bnflin,

      t_j_1bnflin type standard table of type_j_1bnflin,
      t_item_tax  type standard table of j_1bnfstx with header line,
      t_j_1bnfstx type standard table of j_1bnfstx with header line,
      wa_item_tax type j_1bnfstx,
      wa_1bnflin            type type_j_1bnflin,
      wa_item               type j_1bnflin,
      wa_vr_item            like j_1binlin.

    select a~mandt a~docnum a~itmnum a~refkey a~menge a~meins
           a~netpr a~netwr a~tmiss a~netfre a~netins a~netoth
           a~netdis a~incltx
    into table t_j_1bnflin
    from j_1bnflin as a
    for all entries in t_j_1bnfdoc
    where a~docnum = t_j_1bnfdoc-docnum.


    "Busca os impostos de NFs
    select * into table t_j_1bnfstx
      from j_1bnfstx
        for all entries in t_j_1bnfdoc
            where docnum = t_j_1bnfdoc-docnum.
    sort t_j_1bnfstx by docnum itmnum.

    loop at t_j_1bnfdoc assigning <fs_j_1bnfdoc>.

    read table t_j_1bnflin
    with key docnum = <fs_j_1bnfdoc>-docnum assigning <fs_j_1bnflin> binary search.

    "Valor da Nota
    v_tabix_nflin = sy-tabix.
    perform f_valor_nf.

    endloop.

form f_valor_nf.

  clear wa_rel-valor_nf_tot.
  loop at t_j_1bnflin into wa_1bnflin from v_tabix_nflin.

    if wa_1bnflin-docnum ne <fs_j_1bnfdoc>-docnum.
      exit.
    endif.

    move-corresponding wa_1bnflin to wa_item.

    clear wa_vr_item.
    perform f_tabela_taxas. "Impostos do item da nf

    check not t_item_tax[] is initial.

    "Executa função que retorna o valor nf do item
    call function 'J_1B_NF_VALUE_DETERMINATION_I'
      exporting
        nf_item     = wa_item
      importing
        ext_item    = wa_vr_item
      tables
        nf_item_tax = t_item_tax.

    wa_rel-valor_nf_tot = wa_rel-valor_nf_tot + wa_vr_item-nftot.

  endloop.

endform.                    "f_valor_nf

form f_tabela_taxas.

  clear t_item_tax[].
  refresh t_item_tax[].

  read table t_j_1bnfstx
       with key docnum = wa_1bnflin-docnum
                itmnum = wa_1bnflin-itmnum into wa_item_tax binary search.

  check sy-subrc = 0.

  loop at t_j_1bnfstx into wa_item_tax from sy-tabix.

    if wa_item_tax-docnum ne wa_1bnflin-docnum
          or wa_item_tax-itmnum ne wa_1bnflin-itmnum.
      exit.
    endif.

    append wa_item_tax to t_item_tax.

  endloop.

endform.                    "f_tabela_taxas

***************************

DATA: gv_callmode  TYPE flag.

field-symbols <f_callmode> type any.
CONSTANTS: c_assign_callmode(27) TYPE c VALUE '(SAPLJ_1B_NFE)GV_CALLMODE'.

* Alimentando variável para chamar RFC de forma sincrona
ASSIGN (c_assign_callmode) TO <f_callmode>.
        if sy-subrc = 0.
          gv_callmode = 'X'.
          MOVE gv_callmode TO <f_callmode>.
            UNASSIGN <f_callmode>.
        endif.

**************************
ENHANCEMENT-POINT
-----------------

para validações, na J1B1/J1B1N
LJ1BCU06
ENHANCEMENT-POINT J_1B_NF_VALUE_DETERMINATION_01 SPOTS ES_SAPLJ1BC.

Alterar data saída mercadoria (VL02N, VL06*)
ENHANCEMENT-SECTION     lv50lf01_02 SPOTS es_saplv50s.
Include LV50LF01 --- FORM delivery_update_header, da função WS_DELIVERY_UPDATE_2.

**************************
Objetos bloqueio NF:
--------

Transação: ZFI089
Tabelas: ZFIT027 (Tabela exceção para criação NF (Miro/j1b1(n))
ZSDT079 (Tabela parametrização data do fechamento de faturas - VL02N/VF01)

Referente a Miro: 
include ZINVOICE_UPDATE_01 (badi INVOICE_UPDATE), implementação ZINVOICE_UPDATE,
método CHANGE_AT_SAVE.

Referente a J1B1(N):
ENHANCEMENT ZES_SAPLJ1BC (na função J_1B_NF_VALUE_DETERMINATION).

**************************

razão, diário (contabilidade)
T881 - Mestre de ledgers  (nomes das tabelas de ledgers)
FAGLFLEXT  -  Razão: totais
glt0 (antes das versões ecc5)

faglflexa  se for Newledger.

**************************
Programa p/ testar performance de objetos (leituras em tabelas,etc)
RSHOWTIM

**************************
  PERFORM f_upload USING l_filename t_sev_i[].

  FORM f_upload USING p_filename p_tabela TYPE STANDARD TABLE.

     CALL FUNCTION 'GUI_UPLOAD'
       EXPORTING
         filename                = p_filename
       TABLES
         data_tab                = p_tabela


****************** Funções encripta e decripta

FIEB_PASSWORD_DECRYPT
FIEB_PASSWORD_ENCRYPT 

e para o campo da sua tela ficar como caracteres de senha, tente o código abaixo:

REPORT  ZTEST_LOGIN.
parameters: p_user type sy-uname,
                     p_passwd(10). 
 at selection-screen output.
    loop at screen.
     if screen-name = 'P_PASSWD'.
      screen-invisible = 1.
      modify screen.
     endif.
    endloop.

*********************************
Busca servidores ativos:
DATA: BEGIN OF IMSXXLIST OCCURS 10.
        INCLUDE STRUCTURE MSXXLIST_V6.
DATA: END OF IMSXXLIST.

  CALL FUNCTION 'TH_SERVER_LIST'
    TABLES
      LIST           = IMSXXLIST
    EXCEPTIONS
      NO_SERVER_LIST = 1
      OTHERS         = 2.

Campo IMSXXLIST-STATE = '01' (servidor ativo)

----
Retornar dados de memória (SU02)
Função 'SAPTUNE_GET_SUMMARY_STATISTIC'

-------------
RZLLITAB (Tabela com grupo servidores)
  CALL FUNCTION 'MASTERIDOC_CREATE_REQ_CLFMAS'
       STARTING NEW TASK MESTYP
       DESTINATION IN GROUP RZLLITAB-CLASSNAME

*********************************
    CALL 'SYST_LOGOFF'.

*********************************
Bloqueia JOB:
   CALL FUNCTION 'ENQUEUE_ESTBTCO'
        EXPORTING  JOBNAME        = JOBNAME
                   JOBCOUNT       = JOBCOUNT
                   _WAIT          = 'X'
                   _SCOPE         = '1'
        EXCEPTIONS FOREIGN_LOCK   = 1
                   SYSTEM_FAILURE = 2
                   OTHERS         = 99.

------------------

BP_JOB_READ (busca dados de job).

Retorna status do JOB.
  CALL FUNCTION 'BP_JOB_STATUS_GET'
    EXPORTING
      jobcount                         = zinteproces-jobcount
      jobname                          = zinteproces-jobname
    IMPORTING
      status                           =  l_status
*     HAS_CHILD                        =
    EXCEPTIONS
      job_doesnt_exist                 = 1
      unknown_error                    = 2
      parent_child_inconsistency       = 3
      OTHERS                           = 4.

  IF l_status = 'R'.
    MESSAGE w014.
  ELSE.

----------------------
Função que retorna log de jobs (log job):
BP_JOBLOG_READ

************************----------
Excluir dados de memória (export to database):

Obs: Tabelas:
INDX (usada em geral)
RFDT (pra FI)

 data: wa_indx type indx.
 
  export tab = itab to database indx(xy) from wa_indx client
    sy-mandt
    id 'DETAILLIST'.

* to import 
  data: wa_indx type indx.
  import tab = itab from database indx(xy)
   to wa_indx client sy-mandt id 'DETAILLIST'.

* deletes the data to save wastage of memory
    delete from database indx(xy)
      client sy-mandt
      id 'DETAILLIST'.


Exportação de objeto para memória e usando em outro programa que será executado em job:

-------------------------------------------
No programa exporta:
  EXPORT t_relat_x TO DATABASE indx(st) ID 'JOBREC001'.
  EXPORT t_vbeln   TO DATABASE indx(st) ID 'JOBREC002'.
  EXPORT vg_mot_recusa TO DATABASE indx(st) ID 'MOTREC1'.
  EXPORT t_saida_x TO DATABASE indx(st) ID 'SAIREC001'.

No programa importa:
  IMPORT: t_relat_x FROM DATABASE indx(st) ID 'JOBREC001',
          t_vbeln   FROM DATABASE indx(st) ID 'JOBREC002',
          vg_mot_recusa FROM DATABASE indx(st) ID 'MOTREC1',
          t_saida_x FROM DATABASE indx(st) ID 'SAIREC001'.
-------------------------------------------


            EXPORT it_plano_final TO DATABASE indx(st) ID 'PLANOFINAL'.

          CALL FUNCTION 'JOB_OPEN'
            EXPORTING
              jobname          = 'PLANO_PROMOCIONAL'
            IMPORTING
              jobcount         = g_jobcount
            EXCEPTIONS
              cant_create_job  = 1
              invalid_job_data = 2
              jobname_missing  = 3
              OTHERS           = 4.
          IF sy-subrc = 0.
            EXPORT it_plano_final TO DATABASE indx(st) ID 'PLANOFINAL'.

            CALL FUNCTION 'JOB_SUBMIT'
                 EXPORTING
                      authcknam               = sy-uname
                      jobcount                = g_jobcount
                      jobname                 = 'PLANO_PROMOCIONAL'
                      language                = sy-langu
                      report                  = 'ZSDR059'
*            variant                 = vg_variant
                 EXCEPTIONS
                      bad_priparams           = 1
                      bad_xpgflags            = 2
                      invalid_jobdata         = 3
                      jobname_missing         = 4
                      job_notex               = 5
                      job_submit_failed       = 6
                      lock_failed             = 7
                      program_missing         = 8
                      prog_abap_and_extpg_set = 9
                      OTHERS                  = 10.
            IF sy-subrc = 0.
              CALL FUNCTION 'JOB_CLOSE'
                EXPORTING
                  jobcount             = g_jobcount
                  jobname              = 'PLANO_PROMOCIONAL'
                  strtimmed            = 'X'
                EXCEPTIONS
                  cant_start_immediate = 1
                  invalid_startdate    = 2
                  jobname_missing      = 3
                  job_close_failed     = 4
                  job_nosteps          = 5
                  job_notex            = 6
                  lock_failed          = 7
                  OTHERS               = 8.
            ENDIF.
          ENDIF.

*************************
  DATA:
       l_jobname  LIKE tbtcjob-jobname," Job's name
       l_jobcount LIKE tbtcjob-jobcount," Job's number
       l_rele     LIKE btch0000-char1. " Job was processed ?

  DATA: params LIKE pri_params,
        days(1)  TYPE n VALUE 2,
        count(3) TYPE n VALUE 1,
        valid    TYPE c,
        nome_int(18),
        nome_spool(68).

  CALL FUNCTION 'GET_PRINT_PARAMETERS'
              EXPORTING
                   destination    = 'LP01'
                   copies         = count
                   list_name      = sy-uname
                   list_text      = nome_spool
                   immediately    = ' '
                   release        = ' '
                   new_list_id    = 'X'
                   expiration     = days
                   line_size      = 79
                   line_count     = 23
                   layout         = 'X_PAPER'
                   sap_cover_page = 'X'
                   cover_page     = 'X'
*                  receiver       = 'SAP*'
*                  department     = 'System'
                   no_dialog      = 'X'
              IMPORTING
                   out_parameters = params
                   valid          = valid.

*   Se não encontrou dispositivo de saída, aborta processamento
    if valid is initial.
      MESSAGE i000(yo) WITH
      'Não foi encontrado nenhum dispositivo de saída para o usuário.'
      'Favor fixar um dispositivo de saída, em:'
      '<Sistema><Especifs. do usuário><Dados próprios>'.
      exit.
    endif.


  CONCATENATE 'INTERFACE - ' sy-datum+6(2) '/' sy-datum+4(2) '/'
              sy-datum(4) ' - ' sy-uzeit(2) ':' sy-uzeit+2(2) ':'
              sy-uzeit+4(2) ':' INTO l_jobname.

  CALL FUNCTION 'JOB_OPEN'
    EXPORTING
      jobname          = l_jobname
    IMPORTING
      jobcount         = l_jobcount
    EXCEPTIONS
      cant_create_job  = 1
      invalid_job_data = 2
      jobname_missing  = 3
      OTHERS           = 4.

**>>GBE -> Atualiza tabela de controle de processamento com o
**>>número do job para maior controle quando no reprocessamento
  UPDATE zinteproces SET jobcount = l_jobcount
                         jobname  = l_jobname.
  IF sy-subrc EQ 0.
    COMMIT WORK.
  ELSE.
    ROLLBACK WORK.
  ENDIF.
**>>GBE

  SUBMIT zintedis WITH empresa  EQ p_bukrs
                  AND RETURN
                  TO SAP-SPOOL
                  SPOOL PARAMETERS params
                  WITHOUT SPOOL DYNPRO
                  USER sy-uname
                  VIA JOB l_jobname
                  NUMBER  l_jobcount.

*  Pode ser usado assim também, pra chamar o programa
*  CALL FUNCTION 'JOB_SUBMIT'
*    EXPORTING
*      authcknam               = c_uname
*      jobcount                = l_jobcount
*      jobname                 = l_jobname
*      language                = sy-langu
*      priparams               = params
*      report                  = 'zintedis'
*      VARIANT                 = l_variant
*    EXCEPTIONS
*      bad_priparams           = 1
*      bad_xpgflags            = 2
*      invalid_jobdata         = 3
*      jobname_missing         = 4
*      job_notex               = 5
*      lock_failed             = 7
*      program_missing         = 8
*      prog_abap_and_extpg_set = 9
*      OTHERS                  = 10.


  CALL FUNCTION 'JOB_CLOSE'
    EXPORTING
      jobcount             = l_jobcount
      jobname              = l_jobname
      strtimmed            = 'X'
    EXCEPTIONS
      cant_start_immediate = 1
      invalid_startdate    = 2
      jobname_missing      = 3
      job_close_failed     = 4
      job_nosteps          = 5
      job_notex            = 6
      lock_failed          = 7
      OTHERS               = 8.

***********************************

        EXEC SQL.
          select * into :j_1bnfdoc
            from j_1bnfdoc where
            bukrs  = :wa_regcab-nempresa  and
            branch = :j_1filial           and
            docdat = :j1_data_doc         and
            nftype = :aux_nftype          and
            nfnum  = :wa_regcab-num_nf    and
            parvw  = :func_parc           and
            parid  = :j_1clien
        ENDEXEC.

ADBC_DEMO
ADBC_DEMO_LOBS_ORA
ADBC_DEMO_METADATA
ADBC_QUERY
ADBC_TEST_CONNECTION

**********************************************
REPORT ZTESTE.

TABLES: makt.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(33) text_001 FOR FIELD p_matnr.
PARAMETERS: p_matnr LIKE mara-matnr.
SELECTION-SCREEN COMMENT 60(40) w_maktx.
SELECTION-SCREEN END OF LINE.

INITIALIZATION.
text_001 = 'Material:'.

AT SELECTION-SCREEN.

SELECT SINGLE maktx FROM makt
INTO w_maktx
WHERE matnr = p_matnr.

START-OF-SELECTION.

------------------------------------------
selection-screen begin of block b1 with frame.
selection-screen comment /1(72) text-001.
selection-screen comment /1(72) text-002.
selection-screen comment /1(72) text-003.
selection-screen skip 1.
selection-screen comment /1(72) text-004.
selection-screen begin of line.
parameters: p_xxx(1) type c.
selection-screen end of line.
selection-screen end of block b1.

------------------------------------
SELECTION-SCREEN COMMENT /1(50) comm1 MODIF ID mg1.
SELECTION-SCREEN ULINE.
SELECTION-SCREEN SKIP.

SELECTION-SCREEN COMMENT /1(30) comm2.
SELECTION-SCREEN ULINE .
                         "posição, e entre parênteses, tamanho do texto (3).
SELECTION-SCREEN COMMENT /05(3) comm3.  
SELECTION-SCREEN COMMENT  30(3) comm4.
SELECTION-SCREEN COMMENT  55(3) comm5.
SELECTION-SCREEN COMMENT  80(3) comm6.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 05.
PARAMETERS: r1 RADIOBUTTON GROUP rad1.
SELECTION-SCREEN POSITION 30.
PARAMETERS: r2 rADIOBUTTON GROUP rad1.
SELECTION-SCREEN POSITION 55.
PARAMETERS: r3 RADIOBUTTON GROUP rad1.
SELECTION-SCREEN POSITION 80.
PARAMETERS: r4 RADIOBUTTON GROUP rad1.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN ULINE.

AT SELECTION-SCREEN OUTPUT.
  comm1 ='Selection Screen'.
  comm2 ='Select one'.
  
  comm3 = 'RB1'.
  comm4 = 'RB2'.
  comm5 = 'RB3'.
  comm6 = 'RB4'.

**********************************************

    call function 'TRANSACTION_BEGIN'  "94702/KB
         importing                     "94702/KB
              transaction_id = transaction_id                  "94702/KB
         exceptions                    "94702/KB
              others         = 1.      "94702/KB
    call function 'BAPI_PO_CREATE1'     "94702/KB
         exporting                     "94702/KB
              po_header                  = po_head             "94702/KB
         importing                     "94702/KB
              purchaseorder              = h_ebeln             "94702/KB
         tables                        "94702/KB
              po_items                   = po_item             "94702/KB
              po_item_schedules          = po_sched            "94702/KB
              po_item_account_assignment = po_acct             "94702/KB
              po_item_text               = po_text             "94702/KB
              return                     = return              "94702/KB
         exceptions                    "94702/KB
              others                     = 0.                  "94702/KB
    call function 'TRANSACTION_STATUS' "94702/KB
         importing                     "94702/KB
              status  = status_of_transaction                  "94702/KB
         exceptions                    "94702/KB
              others  = 1.             "94702/KB
    if status_of_transaction = 'COMMIT_WORK'.                    "94702/KB
      commit work and wait.            "94702/KB
    endif.                             "94702/KB
    call function 'TRANSACTION_END'    "94702/KB
         exporting                     "94702/KB
              transaction_id = transaction_id                  "94702/KB
         exceptions                    "94702/KB
              others         = 1.      "94702/KB

***********************
DATA: BEGIN OF sel_thead OCCURS 10.
        INCLUDE STRUCTURE thead.
DATA: END OF sel_thead.

*- longtexts ----------------------------------------------------------*
DATA:   BEGIN OF tline OCCURS 30.
        INCLUDE STRUCTURE tline.
DATA:   END OF tline.

DATA:   BEGIN OF htext,
          tdobject     LIKE thead-tdobject,
          tdname       LIKE thead-tdname,
          tdid         LIKE thead-tdid,
          tdspras      LIKE thead-tdspras,
        END OF  htext.

  LOOP AT ekpokey.
* fill keyfields for the texts
    CLEAR htext.
    htext-tdobject = 'EKPO      '.
    htext-tdid     = '*   '.
    htext-tdspras  = ekko-spras.
    htext-tdname(10)   = ekpokey-ebeln.
    htext-tdname+10(5)   = ekpokey-ebelp.

* read all textheaders
    CALL FUNCTION 'SELECT_TEXT'
      EXPORTING
        database_only = 'X'                                 "631310
        object        = htext-tdobject
        name          = htext-tdname
        id            = htext-tdid
        language      = htext-tdspras
      TABLES
        selections    = sel_thead
      EXCEPTIONS
        error_message = 0
        OTHERS        = 1.

* nothing found - information in table return
    READ TABLE sel_thead INDEX 1.
    IF sy-subrc NE 0.
      PERFORM fill_bapireturn TABLES return
                              USING  'I'
                                     'W5'
                                     '034'
                                     ekpokey-ebeln
                                     ekpokey-ebelp
                                     space
                                     space.
      IF 1 = 2. MESSAGE i034(w5) WITH '' ''. ENDIF.
      CONTINUE.
    ENDIF.

* read the several textlines for each text-ID
    LOOP AT sel_thead.
      CLEAR: tline.
      REFRESH: tline.
      CALL FUNCTION 'READ_TEXT'
        EXPORTING
          id            = sel_thead-tdid
          language      = sel_thead-tdspras
          name          = sel_thead-tdname
          object        = sel_thead-tdobject
        TABLES
          lines         = tline
        EXCEPTIONS
          error_message = 0
          OTHERS        = 1.

      IF sy-subrc EQ 0.
        LOOP AT tline.
          po_item_texts-po_number = ekpokey-ebeln.
          po_item_texts-po_item   = ekpokey-ebelp.
          po_item_texts-text_id   = sel_thead-tdid.
          po_item_texts-text_form = tline-tdformat.
          po_item_texts-text_line = tline-tdline.
          APPEND po_item_texts.
        ENDLOOP.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

**************************************************
Visão (view) - J_1BNFDOC - J_1BNFLIN  -----> Campo REFKEY.

**************************************************
refresh nos buffers (transação SWU_OBUF)

***********************
C14ALD_BAPIRET2_SHOW  (mostra popup com mensagens de retorno de bapi).

Bapis /// bapi's

KB21N: BAPI_ACC_ACTIVITY_ALLOC_POST (preencher doc_header-username)
  ou BAPI_ACC_PRIMARY_COSTS_POST.

----
Modificar registro info (me12):
ME_UPDATE_INFORECORD

    move-corresponding: it_eina to xeina,
                        it_eina to yeina,
                        it_eina to reg_eina,
                        it_eine to xeine,
                        it_eine to yeine.

    xeina-meins = p_meins.   "Campo a ser alterado...

    call function 'ME_UPDATE_INFORECORD'
      exporting
        xeina    = xeina
        xeine    = xeine
        yeina    = yeina
        yeine    = yeine
        reg_eina = reg_eina.

---------------------------------------------
Cancelar lote de controle (QA02)
CALL FUNCTION 'QPLEXT_INSPECTION_LOT_CANCEL'

---------------------------------------------
Função VENDOR_UPDATE: XK02, modificação fornecedor em geral.
---------------------------------------------
BAPI_SALESDOCUMENT_COPY  (Criação de ordem de venda)
---------------------------------------------
ME_UPDATE_DOCUMENT (altera pedido de compra) -- ME22N
Campo trtyp (V - mnodificando)  Aktyp
---------------------------------------------

Bapi  BAPI_REQUIREMENT_CREATE (ME51N)                  ------- badi MD_PURREQ_CHANGE~CHANGE_BEFORE_SAVE_MRP
      BAPI_REQUISITION_CREATE (também cria requisição) ------- user exit EXIT_SAPLMEWQ_001
---------
"Determinar estratégia -- Início
IF T399D-XFAN3 IS INITIAL.
   MOVE XFLAG TO AU61X-FLG01.
ELSE.
   CLEAR AU61X-FLG01.
ENDIF.
CALL FUNCTION 'ME_REL_STRATEGY_REQUISITION'
     EXPORTING
          I_EBAN_NEW  =  EBAN
          I_EBAN_OLD  = *EBAN
          I_EBKN_NEW  =  EBKN
          I_EBKN_OLD  = *EBKN
          I_XCLASS    =  AU61X-FLG01
*         i_event     = 1                    "4.0C       "Einzel-BAnf
     IMPORTING
          E_EBAN      = EBAN.

*--> set processing state (note 878790)
IF EBAN-FRGST NE SPACE.
  IF EBAN-BANPR IS INITIAL OR EBAN-BANPR = '02'.
    EBAN-BANPR = '03'.
  ENDIF.
ELSE.
  EBAN-BANPR = '02'.
ENDIF.

*BEGIN NOTE 762222
IF EBAN-FRGRL = 'X' and EBAN-BANPR = '05'.
   EBAN-BANPR = '03'.
ENDIF.
*END NOTE 762222
"Determinar estratégia -- Fim

---------------------------------------------
Transação KP26 (planejamento atividades/tarifas)
BAPI_COSTACTPLN_POSTACTOUTPUT 

Tem um programa standard chamado ‘RK_BAPI_POSTACTOUTPUT’ que simula esta bapi.
---------------------------------------------

Bapi FB01.
BAPI_ACC_DOCUMENT_POST 

FB08 --- BAPI_ACC_DOCUMENT_REV_POST (Documento contábil reverso).


--- Anular compensação (FBRA):
  DATA:
    STODT LIKE BKPF-BUDAT,           "Storno-Buchungsdatum und -Periode
    STOMO LIKE BKPF-MONAT.           "zur Übergabe an FBRA (RWIN-Aufruf)

  CALL FUNCTION 'CALL_FB08'          "Simulation des Stornos
       EXPORTING
            I_BUKRS      = I_BUKRS
            I_BELNR      = I_AUGBL
            I_GJAHR      = I_GJAHR
            I_STGRD      = I_STGRD
            I_BUDAT      = I_BUDAT
            I_XSIMU      = 'X'
       IMPORTING
            E_BUDAT      = STODT
            E_MONAT      = STOMO
       EXCEPTIONS
            NOT_POSSIBLE = 4.
  IF SY-SUBRC <> 0.
    RAISE NOT_POSSIBLE_FB08.
  ELSE.
    CALL FUNCTION 'CALL_FBRA'
       EXPORTING
            I_BUKRS      = I_BUKRS
            I_AUGBL      = I_AUGBL
            I_GJAHR      = I_GJAHR
            I_STODT      = STODT
            I_STOMO      = STOMO
       EXCEPTIONS
            NOT_POSSIBLE = 4.
    IF SY-SUBRC <> 0.
      RAISE NOT_POSSIBLE_FBRA.
    ELSE.
      CALL FUNCTION 'CALL_FB08'
         EXPORTING
              I_BUKRS      = I_BUKRS
              I_BELNR      = I_AUGBL
              I_GJAHR      = I_GJAHR
              I_STGRD      = I_STGRD
              I_BUDAT      = I_BUDAT
         EXCEPTIONS
              NOT_POSSIBLE = 4.
      IF SY-SUBRC <> 0.
        RAISE NOT_POSSIBLE_FB08.
      ENDIF.
    ENDIF.
  ENDIF.

--------
FB1S:
PERFORM z_preenche_tela USING: 'X' 'SAPMF05A' '0131',
                                  ' ' 'RF05A-AGKON' vl_agkon,
                                  ' ' 'BKPF-BUKRS' vl_bukrs,
                                  ' ' 'BKPF-WAERS' vl_waers,
                                  ' ' 'BKPF-BUDAT' vl_datatxt,
                                  ' ' 'BKPF-MONAT' vl_monat,
                                  ' ' 'BDC_OKCODE' '/11'.

  PERFORM z_preenche_tela USING: 'X' 'SAPMF05A' '0700',
                                  ' ' 'BDC_OKCODE' '/11'.

* Exporte para a memória com o ID %F124%.
  EXPORT ybsis TO MEMORY ID '%F124%'.
* Executa a transação, FB1S. Obs: Só funciona no módulo N
* monta os parametros
  tg_params-dismode = 'N'.
  tg_params-updmode = 'S'.
  tg_params-racommit = 'X'.

  CALL TRANSACTION 'FB1S' USING tg_bdc
        OPTIONS FROM tg_params
        MESSAGES INTO tg_messtab.

---------
FB05, FB41, FBB1, fbcb...
Bapi FB05 (compensação de documentos)
  sy-tcode = 'ZFI239'.
  "Realiza compensação...
    "i_auglv (Operação a processar):
    "AUSGZAHL = Saída de pagamentos
    "EINGZAHL = Entrada de pagamentos
    "GUTSCHRI = Crédito
    "UMBUCHNG = Transferência c/compensação

  call function 'POSTING_INTERFACE_CLEARING'
    exporting
      i_tcode   = 'FB05'
      i_auglv   = 'UMBUCHNG'    "call transaction (batch input)
    importing
      e_subrc   = v_subrc
      e_msgid   = sy-msgid
      e_msgty   = sy-msgty
      e_msgno   = sy-msgno
      e_msgv1   = sy-msgv1
      e_msgv2   = sy-msgv2
      e_msgv3   = sy-msgv3
      e_msgv4   = sy-msgv4
    tables
      t_ftpost  = t_ftpost
      t_blntab  = t_xblntab
      t_ftclear = t_ftclear
      t_fttax   = t_xfttax
    exceptions
      others    = 4.

  if sy-subrc ne 0 or v_subrc ne 0.  "Erro no lançamento...
    perform f_trata_mensagem.
  else.
    "Documento gerado com sucesso.
    perform f_grava_log using text-007 c_ok sy-msgid sy-msgno t_doc_juros-bukrs sy-msgv1(10) sy-datum(4).
  endif.

Obs: Caso o batch input (acima) não funcionar, pode ser que seja devido a campos ocultos na FB05. A rotina abaixo trata isso.
É um enhancement na include MF05AFB0_BUCHUNGSZEILE_MERKEN_, no final do form buchungszeile_merken_auttab.

  data: l_fb05(1)     type c,
        l_feldauswahl type tbsl-faus1.

  clear l_fb05.
  "Se a FB05 foi chamada pela função ZFI_LANCA_ADIANTAMENTO...
  import mfb05 = l_fb05 from memory id 'ZM_FB05'.

  if l_fb05 = 'X'. "FB05 foi chamada pela função ZFI_LANCA_ADIANTAMENTO.
     "A rotina abaixo é para verificar se somente os campos MATNR (material)
     "e WERKS (centro) são requiridos.
     l_feldauswahl = feldauswahl.
     l_feldauswahl+8(1) = '.'.   "campo MATNR
     l_feldauswahl+25(1) = '.'.  "campo WERKS
     if l_feldauswahl CA '+'.  "contém mais campos com atributo obrigatório
     else.
        "não possui mais nenhum campo requerido, além do MATNR e WERKS.
        refresh auttab[]. "Libera execução direta, sem ter que dar <enter>(s)
     endif.
  endif.

---------------

F-30 BAPIS BAPI_ACC_BILLING_POST ou BAPI_ACC_PYMNTBLK_UPDATE_POST ou POSTING_INTERFACE_CLEARING.

POSTING_INTERFACE_START
POSTING_INTERFACE_CLEARING
POSTING_INTERFACE_END 
------ 
Para alterar/preencher campo TCODE (cabeçalho documento contábil):
Criar enhancement, na include LACC4F20, no começo do 'form fill_glvor'.
  if ( sy-cprog = 'ZCOR011' )
          and ( sy-tcode is initial ).
     it_acchd-tcode = sy-cprog.
  endif.

-----------------------------------
Bapi VL02N:
BAPI_OUTB_DELIVERY_CHANGE
      call function 'BAPI_OUTB_DELIVERY_CHANGE'
        exporting
          header_data    = wa_header_data
          header_control = wa_header_control
          delivery       = t_likp-vbeln
          techn_control  = wa_techn_control
        tables
          item_data      = it_item_data
          item_control   = it_item_control
          item_data_spl  = it_item_data_spl
          return         = it_return.

BAPI_INB_DELIVERY_CHANGE
Pra estornar(eliminar):
        header_data-deliv_numb = tl_log-z_ar.
        header_ctrl-deliv_numb = tl_log-z_ar.
        header_ctrl-dlv_del    = 'X'.

LE_SHP_GOODSMOVEMENT (Alterar MKPF - cabeçalho docto material).

---Bapi Criação NF(BAPI_J_1B_NF_CREATEFROMDATA)----------

Você terá que usar a função abaixo, para ativar a fila de impressão na NAST:

     CALL FUNCTION 'J_1B_NF_DOCUMENT_PRINT' IN UPDATE TASK
           EXPORTING
                doc_number         = docnum
           EXCEPTIONS
                document_not_found = 6
                messaging_problem  = 7
                OTHERS             = 9.

       COMMIT WORK.
--------------------------------------------------------
VT01N
BAPI_SHIPMENT_CREATE (criação de documento de transporte) 

VT02N
BAPI_SHIPMENT_CHANGE (modifição de documento de transporte)

BAPI_SHIPMENT_DELETE (exclusão de documento de transporte)
---------------------------------------------------
VL??:
(Cria a Remessa - recebimento mercadoria) 'BAPI_OUTB_DELIVERY_CREATE_STO'.
(Altera a remessa criada - atualiza depósito) 'BAPI_OUTB_DELIVERY_CHANGE'.
------------------
VF01 --  CALL FUNCTION 'RV_INVOICE_CREATE'

Ponto de mensagens (erro) que vão para log standard, sem abortar faturamentos em massa:
Include RV60AFZC (vl06*), form userexit_number_range_inv_date.

  data: l_msg1  type  char34,
        l_msg2  type  char34,
        l_msg3  type  char34,
        l_subrc type  subrc.

  call function 'ZFTRA_VALIDA_FATURA'
    exporting
      i_vbak  = vbak
      i_vbap  = vbap
      i_likp  = likp
      i_lips  = lips
      i_vbrk  = vbrk
      i_vbrp  = vbrp
    importing
      e_msg1  = l_msg1
      e_msg2  = l_msg2
      e_msg3  = l_msg3
      e_subrc = l_subrc
    tables
      t_xvbfs = xvbfs[]       "tabela interna standard com registros de erros
    changing
      i_vbsk  = vbsk.

  if l_subrc ne 0.    "Não autorizou faturamento...
    rc = 8.
  endif.

A função 'ZFTRA_VALIDA_FATURA' está na pasta C:\Lacb\abap\Programas_Reports\apartirdeabril_2010

RV60B900 (também tem rotinas que gravam mensagem em log, conforme acima)

------------------
Estornar movimentação mercadoria  (VL09)
        SELECT vbeln mjahr erdat erzet
              INTO CORRESPONDING FIELDS OF TABLE t_vbfa
              FROM vbfa
              WHERE vbelv = v_fornec AND
                    plmin = '+' AND
                    vbtyp_N = 'R'
              GROUP BY vbeln mjahr posnn erdat erzet.


          SORT t_vbfa by erdat DESCENDING
                         erzet DESCENDING.

          READ TABLE t_vbfa into wa_vbfa INDEX 1.

          CALL FUNCTION 'WS_REVERSE_GOODS_ISSUE'
            EXPORTING
              I_VBELN                   = v_fornec
              I_BUDAT                   = sy-datlo
              I_MBLNR                   = wa_vbfa-vbeln
              I_TCODE                   = 'VL09'
              I_VBTYP                   = wa_vbuk-vbtyp
              I_MJAHR                   = wa_vbfa-mjahr
            TABLES
              T_MESG                    = T_MESG
            EXCEPTIONS
              ERROR_REVERSE_GOODS_ISSUE = 1
              OTHERS                    = 2.

------------------
BAPI_GOODSMVT_CREATE (MIGO)  ------ GM_CODE ----> tabela T158G

    CALL FUNCTION 'MB_CHANGE_DOCUMENT' IN UPDATE TASK
      TABLES
        zmkpf = xmkpf
        zmseg = xmseg.

------------------------------------------------------------------------
VI01 (custo de frete)

PARAMETERS pacsz LIKE rv54a-pacsz DEFAULT 1
                                  MODIF ID frc   "Package size
                                  NO-DISPLAY.    "Currently disabled

DATA: object_range TYPE v54a0_ref_tab WITH HEADER LINE,
      object_range_locked TYPE v54a0_ref_tab WITH HEADER LINE.

*       Prepare interface of 'SD_SCDS_CREATE'
        REFRESH object_range.
        object_range-vbtyp = vbtyp_transport.
        LOOP AT zvttk.
          object_range-rebel = zvttk-tknum.
          APPEND object_range.
        ENDLOOP.

*       Create shipment cost documents
        CALL FUNCTION 'SD_SCDS_CREATE'
          EXPORTING
            i_opt_package_size    = pacsz
          IMPORTING
            e_refobj_range_locked = object_range_locked[]
          CHANGING
            c_refobj_range        = object_range[]
          EXCEPTIONS
            OTHERS                = 0.

-------
VI02 (custo de frete)

FORM estornar_custo_frete  USING    p_tknum TYPE tknum
                                    p_datum TYPE datum.

  DATA: wa_t180       TYPE t180,
        wa_scd        TYPE v54a0_scd,
        wa_refobj     TYPE v54a0_refobj_tab ,
        wa_item       TYPE v54a0_scd_item,
        wa_scd_change TYPE v54a0_scdd,
        it_scd_change TYPE v54a0_scdd_tab.

  " Verifica se ha documento de fretes para deletar
  READ TABLE it_vfkp WITH KEY rebel = p_tknum.
  IF sy-subrc IS INITIAL.

    CLEAR it_vfkk.
    READ TABLE it_vfkk WITH KEY fknum = it_vfkp-fknum.

    CLEAR: wa_scd, wa_scd_change, wa_refobj.
    FREE:  it_scd_change.

    " Ler o documento
    CALL FUNCTION 'SD_SCD_READ'
      EXPORTING
        i_fknum          = it_vfkk-fknum
        i_t180           = wa_t180
        i_opt_refobj     = 'X'
      CHANGING
        c_scd            = wa_scd
        c_refobj_tab     = wa_refobj
      EXCEPTIONS
        scd_not_found    = 1
        refobj_not_found = 2
        refobj_lock      = 3
        delivery_missing = 4
        OTHERS           = 5.

    IF sy-subrc IS INITIAL.

      " Verifica se o documento de custo de frete precisa ser estornado.
      IF wa_scd-vfkk-stabr NE 'A'.

        wa_scd_change-fknum  = it_vfkk-fknum.
        wa_scd_change-change = 'X'.
        wa_scd_change-y = wa_scd.

        " Marcar item do SCD para estorno.
        LOOP AT wa_scd-item INTO wa_item.
          wa_scd-vfkk-fkposl  = wa_item-vfkp-fkpos.
          IF NOT wa_item-vfkp-ebeln IS INITIAL.
            wa_scd-vfkk-fkposl  = wa_item-vfkp-fkpos.
            wa_item-vfkp-updkz  = 'U'.
            wa_item-vfkp-fksto  = 'X'.
            wa_item-vfkp-slstor = 'X'.
            wa_item-vfkp-stdat  = p_datum.
            MODIFY wa_scd-item FROM wa_item INDEX sy-tabix.
          ENDIF.
        ENDLOOP.

        wa_scd_change-x = wa_scd.
        APPEND wa_scd_change TO it_scd_change.

        " Alterar documento de custo de frente estornando os itens transmitidos.
        CALL FUNCTION 'SD_SCDS_SAVE'
          EXPORTING
            i_t180                    = wa_t180
            i_opt_update_task         = 'X'
            i_opt_release_with_dialog = space
            i_refobj_tab              = wa_refobj
          CHANGING
            c_scd_tab                 = it_scd_change
          EXCEPTIONS
            no_change                 = 1
            no_save                   = 2
            OTHERS                    = 3.

        IF sy-subrc IS INITIAL.

          COMMIT WORK AND WAIT.

          " Ler novamente o documento de fretes para atualizar os satus.
          CLEAR: wa_scd, wa_refobj.

          " Aguarda o sistema liberar o documento
          PERFORM verifica_dispo_custo USING it_vfkk-fknum.

          CALL FUNCTION 'SD_SCD_READ'
            EXPORTING
              i_fknum          = it_vfkk-fknum
              i_t180           = wa_t180
              i_opt_refobj     = 'X'
            CHANGING
              c_scd            = wa_scd
              c_refobj_tab     = wa_refobj
            EXCEPTIONS
              scd_not_found    = 1
              refobj_not_found = 2
              refobj_lock      = 3
              delivery_missing = 4
              OTHERS           = 5.

      PERFORM deletar_custo_frete USING  p_tknum
                                         it_vfkk-fknum
                                         wa_scd
                                         wa_refobj.


---
FORM deletar_custo_frete  USING p_tknum  TYPE tknum
                                p_fknum  TYPE fknum
                                p_scd    TYPE v54a0_scd
                                p_refobj TYPE v54a0_refobj_tab .

  DATA: wa_refobj     TYPE v54a0_refobj,
        wa_t180       TYPE t180,
        wa_item       TYPE v54a0_scd_item,
        wa_scd_delete TYPE v54a0_scdd,
        it_scd_delete TYPE v54a0_scdd_tab..

  READ TABLE p_refobj INTO wa_refobj INDEX 1.
  wa_scd_delete-fknum  = p_fknum.
  wa_scd_delete-change = space.
  wa_scd_delete-x      = p_scd.
  wa_scd_delete-y      = p_scd.

  LOOP AT wa_scd_delete-x-item INTO wa_item.
    wa_scd_delete-x-vfkk-fkposl = wa_item-vfkp-fkpos.
  ENDLOOP.

  LOOP AT wa_scd_delete-y-item INTO wa_item.
    wa_scd_delete-y-vfkk-fkposl = wa_item-vfkp-fkpos.
  ENDLOOP.

  APPEND wa_scd_delete TO it_scd_delete.

  CALL FUNCTION 'SD_SCD_DELETE'
    EXPORTING
      i_refobj           = wa_refobj
    CHANGING
      c_scd_wa           = wa_scd_delete
    EXCEPTIONS
      item_not_deletable = 1
      scd_not_deletable  = 2
      OTHERS             = 3.

  IF sy-subrc IS INITIAL.

    CALL FUNCTION 'SD_SCD_CHANGE'
      CHANGING
        c_scd_wa  = wa_scd_delete
        c_scd_tab = it_scd_delete.

    CALL FUNCTION 'SD_SCDS_CHECK_COMPLETE'
      EXPORTING
        i_scd_tab = it_scd_delete
      EXCEPTIONS
        OTHERS    = 99.

    IF sy-subrc IS INITIAL.
      CALL FUNCTION 'SD_SCDS_SAVE'
        EXPORTING
          i_t180                    = wa_t180
          i_opt_update_task         = 'X'
          i_opt_release_with_dialog = space
          i_refobj_tab              = p_refobj
        CHANGING
          c_scd_tab                 = it_scd_delete
        EXCEPTIONS
          no_change                 = 1
          no_save                   = 2
          OTHERS                    = 3.

      IF sy-subrc IS INITIAL.
        COMMIT WORK AND WAIT.
      ELSE.

---
FORM verifica_dispo_custo  USING p_fknum TYPE fknum.

  DATA: l_count TYPE i.

  DO.
    ADD 1 TO l_count.

    CALL FUNCTION 'ENQUEUE_EVVFKKE'
      EXPORTING
        mode_vfkk      = 'E'
        mandt          = sy-mandt
        fknum          = p_fknum
        x_fknum        = ' '
        _scope         = '2'
        _wait          = ' '
        _collect       = ' '
      EXCEPTIONS
        foreign_lock   = 1
        system_failure = 2
        OTHERS         = 3.

    IF sy-subrc IS INITIAL OR
       l_count  GT 10000. " Para nao ficar travado.

      CALL FUNCTION 'DEQUEUE_EVVFKKE'
        EXPORTING
          mode_vfkk = 'E'
          mandt     = sy-mandt
          fknum     = p_fknum
          x_fknum   = ' '
          _scope    = '3'
          _synchron = ' '
          _collect  = ' '.
      EXIT.
    ENDIF.

  ENDDO.

ENDFORM.

---------------------------------------
Bapi ASSET_MASTERRECORD_MAINTENANCE
AS01/AS02 (manutenção de imobilizado)
Parâmetros:
I_TRTYP = 'H' ----> incluir
I_TRTYP = 'V' ----> modificar
I_TRTYP = 'L' ----> deletar
Preencher I_ANLA
          I_ANLAX (se for modificação)

User exit para modificações de campos de imobilizado (qdo MIRO / AS02):
EXIT_SAPLAIST_002
---------------------------------------
Bapi Miro

*  data: l_headerdata type bapi_incinv_create_header,
*        l_belnr type re_belnr.
*
*  data: t_itemdata type table of bapi_incinv_create_item,
*        t_return   type table of bapiret2.
*
*  data: w_mseg type mseg,
*        w_item type bapi_incinv_create_item,
*        v_mode type c value 'A'.
*
*  data: t_bdc type table of bdcdata.
*
*
*  read table it_mseg into w_mseg index 1.
**  read table t_nfitems index 1.
**  select single bukrs waers
**    into (ekko-bukrs, ekko-waers)
**    from ekko
**    where ebeln = t_nfitems-ebeln.
**
**  if ekko-waers is initial.
**    w_nfdoc-waers = ekko-waers.
**  endif.
*
*  l_headerdata-invoice_ind  = 'X'.
*  l_headerdata-doc_date     = sy-datum.
*  l_headerdata-pstng_date   = sy-datum.
*  l_headerdata-ref_doc_no   = is_mkpf-bktxt.
*  l_headerdata-comp_code    = w_mseg-bukrs.
*  l_headerdata-currency     = w_mseg-waers.
** l_headerdata-gross_amount = is_mkpf-xblnr.
*  l_headerdata-calc_tax_ind = 'X'.
*  l_headerdata-header_txt   = 'Teste entr. automática'.
*
*  loop at it_mseg into w_mseg.
*** ..............................
*    w_item-invoice_doc_item = sy-tabix.
*    w_item-po_number        = w_mseg-ebeln.
*    w_item-po_item          = w_mseg-ebelp.
*    w_item-ref_doc          = is_mkpf-mblnr.
*    w_item-ref_doc_year     = sy-datum(4).
*    w_item-ref_doc_it       = sy-tabix.
**   w_item-item_amount      = '999.99'.
*    w_item-po_unit          = w_mseg-meins.
*    w_item-quantity         = w_mseg-menge.
*    w_item-tax_code         = w_mseg-mwskz.                " Get tax code
*    append w_item to t_itemdata.
*  endloop.
*
** SET UPDATE TASK LOCAL
*  call function 'BAPI_INCOMINGINVOICE_CREATE'
**   IN UPDATE TASK
*    exporting
*      headerdata       = l_headerdata
*    importing
*      invoicedocnumber = l_belnr
**     fiscalyear       = w_fiscalyear
*    tables
*      itemdata         = t_itemdata
*      return           = t_return.
*
*  commit work and wait.

Bapi estorno MIRO: BAPI_INCOMINGINVOICE_CANCEL

--------------------
Bapi criação docto inventário (tabela ISEG):

'BAPI_MATPHYSINV_CREATE'

DATA: t_head    TYPE bapi_physinv_create_head.
DATA: t_items   LIKE bapi_physinv_create_items OCCURS 0 WITH HEADER LINE.
DATA: t_max_rec TYPE maxpo.
DATA: t_return  LIKE   bapiret2 OCCURS 0 WITH HEADER LINE.

  t_head-plant        = v_werks.
  t_head-stge_loc     = v_lgort.
  t_head-doc_date     = sy-datum.
  t_head-plan_date    = v_zldat.

  t_items-material = v_matnr.
  t_items-batch    = v_charg.
  t_items-stock_type = v_bstar.
  APPEND: t_items.

  t_max_rec = 99999.
  CALL FUNCTION 'BAPI_MATPHYSINV_CREATE'
    EXPORTING
      head      = t_head
      max_items = t_max_rec
    TABLES
      items     = t_items
      return    = t_return.

---------------------------------------------------
Função para criar/modificar avisos de pagamento (FF63/FF6A)

data: t_fdes        like fdes     occurs 20  with header line.

  "Prepara tabela interna para execução de função standard
  "que altera o aviso de pagamento
  select * from fdes
       into table t_fdes
         where bukrs = i_bukrs and
               zuonr = i_ebeln and
               idenr = v_idenr.

  check sy-subrc = 0.

  read table t_fdes index 1.

  "Verifica se houve modificação de data ou valor do contrato
  check ( t_fdes-datum ne i_kdate )
          or ( t_fdes-wrshb ne i_ktwrt ).

  "Grava mais uma linha na tabela interna t_fdes (com as modificações)
  t_fdes-datum = i_kdate.
  t_fdes-dmshb = i_ktwrt.
  t_fdes-wrshb = i_ktwrt.
  append t_fdes.

  "Executa função standard que altera o aviso de pagamento
  call function 'CASH_FORECAST_MEMO_RECORD_UPD'
    exporting
      aktion   = '2'     "1 - Criar   2 - MODIFICAR 
    tables
      tab_fdes = t_fdes.

--------------------------------
BAPI_SALESORDER_SIMULATE (função para determinação de Preços de SD)

--------------------------------
BAPI Nota de Seviço.

Podes usar a BAPI_ALM_NOTIF_CREATE. 

Depois de executar esta BAPI, tens que executar a BAPI_ALM_NOTIF_SAVE, para efetivar a gravação.

Se der erro use a BAPI_TRANSACTION_ROLLBACK.

------------------------------
Modificar imobilizados (AS92/as02)
  v_companycode          = tab_drseg-bukrs.
  v_asset                = tab_drseg-anln1.
  v_subnumber            = tab_drseg-anln2.
  v_originx-manufacturer = 'X'.

  call function 'BAPI_FIXEDASSET_CHANGE'
    exporting
      companycode = v_companycode
      asset       = v_asset
      subnumber   = v_subnumber
      origin      = v_origin
      originx     = v_originx
    importing
      return      = v_return.

--
  MOVE: '01' TO wa_deparea-area,
*       ''   TO wa_deparea-dep_key.
        ''   TO wa_deparea-odep_start_date, " Data de início do cálculo da depreciação
        ''   TO wa_deparea-sdep_start_date. " Data de início da depreciação especial

  MOVE: '01' TO wa_depareax-area,
*       'X' TO wa_depareax-dep_key,
        'X' TO wa_depareax-odep_start_date, " Data de início do cálculo da depreciação
        'X' TO wa_depareax-sdep_start_date. " Data de início da depreciação especial


  CALL FUNCTION 'BAPI_FIXEDASSET_CHANGE'
    EXPORTING
      companycode                = i_bukrs
      asset                      = i_anln1
      subnumber                  = i_anln2
   IMPORTING
     return                     = tl_return
   TABLES
     depreciationareas          = tl_deparea
     depreciationareasx         = tl_depareax
*   INVESTMENT_SUPPORT         =
*   EXTENSIONIN                =
            .



----------------------------------------
Função/bapi alteração de itens de lançamentos contábeis em massa (fb02).

Obs: Não é necessário o comando 'commit', após execução da função.

REPORT ZTEST_FB02 .

type-pools : TPIT.

DATA : W_BSEG TYPE BSEG,
W_message TYPE T100-TEXT.

data : it_errtab TYPE tpit_t_errdoc WITH HEADER LINE,
it_fldtab TYPE tpit_t_fname WITH HEADER LINE,
it_buztab TYPE tpit_t_buztab WITH HEADER LINE.


Field name to be changed 
it_fldtab-fname = 'ZUONR'.
append it_fldtab.
clear it_fldtab.


Field value 
W_BSEG-ZUONR = 'TEST19'.

ITEM 7 
select single
bukrs
belnr
gjahr
buzei
koart
umskz
bschl
mwart
mwskz
from bseg
into corresponding fields of it_buztab
where belnr = '0902001922' and
bukrs = '3160' and
gjahr = '2006' and
BUZEI = '007'.
if sy-subrc = 0.
APPEND it_buztab.
clear it_buztab.
endif.

CALL FUNCTION 'FI_ITEMS_MASS_CHANGE'
EXPORTING
S_BSEG = W_bseg
IMPORTING
ERRTAB = it_errtab[]
TABLES
IT_BUZTAB = it_buztab
IT_FLDTAB = it_fldtab
EXCEPTIONS
BDC_ERRORS = 1
OTHERS = 2.
IF SY-SUBRC 0.

MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO 
WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4. 
perform generate_message.
write : w_message.
else.
write : 'Updated successfully'.
ENDIF.
&---------------------------------------------------------------------

& Form generate_message 
&---------------------------------------------------------------------
FORM generate_message .
CLEAR W_message.

read table it_errtab index 1.
if sy-subrc 0.
CALL FUNCTION 'FORMAT_MESSAGE'
EXPORTING
id = it_errtab-ERR-MSGID
lang = sy-langu
no = it_errtab-ERR-MSGNR
v1 = it_errtab-ERR-MSGV1
v2 = it_errtab-ERR-MSGV2
v3 = it_errtab-ERR-MSGV3
v4 = it_errtab-ERR-MSGV4
IMPORTING
msg = W_message
EXCEPTIONS
not_found = 1
OTHERS = 2.
endif.

ENDFORM. " generate_message

*********************************
Partidas em aberto ou compensado (fechado)
BAPI_AR_ACC_GETBALANCEDITEMS (Itens compensados para clientes) - como a transação FBL5N.
BAPI_AP_ACC_GETBALANCEDITEMS (Itens compensados para fornecedores) - como a transação FBL1N.
BAPI_AR_ACC_GETOPENITEMS (Itens em aberto para clientes)
BAPI_AP_ACC_GETOPENITEMS (Itens em aberto para fornecedores)

*********************************
Usando tabela de extensões em bapi's de lançamentos contábeis (para campos que não estão nas estruturas, ou modificações em campos existentes).
Nota sap: 487722.
BAPIs:
BAPI_ACC_DOCUMENT_POST         BAPI_ACC_DOCUMENT_CHECK
BAPI_ACC_GL_POSTING_POST       BAPI_ACC_GL_POSTING_CHECK
BAPI_ACC_BILLING_POST          BAPI_ACC_BILLING_CHECK
BAPI_ACC_INVOICE_RECEIPT_POST  BAPI_ACC_INVOICE_RECEIPT_CHECK
BAPI_ACC_GOODS_MOVEMENT_POST   BAPI_ACC_GOODS_MOVEMENT_CHECK

User exit: EXIT_SAPLACC4_001

Para a bapi BAPI_ACC_DOCUMENT_POST, usar a bte função SAMPLE_INTERFACE_RWBAPI01.
Call menu path "Settings -> Process function modules > ...of a customer" enter process = RWBAPI01, the function module created and the product.

Exemplo (BAPI_ACC_DOCUMENT_POST):

  "As estruturas da bapi BAPI_ACC_DOCUMENT_POST não possuem os campos: REBZG, REBZJ, REBZZ.
  "Por isso vamos usar na bapi, a tabela de extensão EXTENSION1, pra levar os campos e seus valores...
  "Esta tabela de extensão EXTENSION1 será lida na função bte ZSAMPLE_INTERFACE_RWBAPI01.
  refresh it_extension.
  wa_extension-field1(4)     = 'BAPI'.     "Chave para leitura
  wa_extension-field1+4(3)   = '001'.      "Nr item
  wa_extension-field1+7(10) = p_rebzg.     "Nr fatura relacionada
  wa_extension-field1+17(4) = p_rebzj.     "Exercício fatura relacionada
  wa_extension-field1+21(3) = p_rebzz.     "Nr item fatura relacionada
  append wa_extension to it_extension.

  sy-tcode = 'ZFI239'.
  call function 'BAPI_ACC_DOCUMENT_POST'
    exporting
      documentheader = gd_header
    importing
      obj_type       = v_obj_type
      obj_key        = v_obj_key
      obj_sys        = v_obj_sys
    tables
      accountgl      = it_accountgl
      accountpayable = it_payable
      currencyamount = it_currency
      extension1     = it_extension
      return         = it_bapiret.

Na função bte abaixo...
FUNCTION ZSAMPLE_INTERFACE_RWBAPI01.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      IT_ACCIT STRUCTURE  ACCIT
*"      IT_ACCCR STRUCTURE  ACCCR
*"      RETURN STRUCTURE  BAPIRET2
*"      EXTENSION STRUCTURE  BAPIACEXTC
*"      IT_ACCWT STRUCTURE  ACCIT_WT
*"  CHANGING
*"     VALUE(DOCUMENT_HEADER) LIKE  ACCHD STRUCTURE  ACCHD
*"----------------------------------------------------------------------
  "As estruturas da bapi BAPI_ACC_DOCUMENT_POST não possuem os campos: REBZG, REBZJ, REBZZ.
  "Por isso vamos usar na bapi, a tabela de extensão EXTENSION1, prenchida no programa
  "ZFIB008 (Lançamento documento contábil de juros a pagar).

  "Busca linha inserida na tabela de extensão (na bapi do programa ZFIB008)...
*  read table extension into extension with key field1(4) = 'BAPI'.
  read table extension with key field1(4) = 'BAPI'.
  if sy-subrc = 0.
    "lê o item relacionado na tabela interna standard de itens...
    read table it_accit with key posnr = extension-field1+4(3).
    if sy-subrc = 0.
      "Atualiza os campos: REBZG, REBZJ, REBZZ.
      it_accit-rebzg = extension-field1+7(10).   "Nr fatura relacionada
      it_accit-rebzj = extension-field1+17(4).   "Exercício fatura relacionada
      it_accit-rebzz = extension-field1+21(3).   "Nr item fatura relacionada
      modify it_accit index sy-tabix.
    endif.
  endif.


*********************************

Adicionar Botões (botão) em barra de ferramentas ALV.

Add custom button onto ALV Grid in SAP

As an ABAPer, you must have thought of adding your custom button to the standard ALV Grid called in your custom program. Here are the steps for achieving this goal.

Custom button on ALV

TO achieve this you should copy the ‘STANDARD’ GUI status from program SAPLKKBL using transaction SE90 –>Programming SubObjects–> Gui Status.

Steps:

1). Using SE80/SE41 you can copy a GUI status from one program to another. It mentions which one in the FM’s help.
2). Create a form named like so:

Code:
*****************************************************************
* Form Set_pf_status
* Notes: Called by FM REUSE_ALV_GRID_DISPLAY
*****************************************************************
FORM set_pf_status USING rt_extab TYPE slis_t_extab.
  SET PF-STATUS ‘ZSTANDARD’.
ENDFORM. “Set_pf_status
 

In the above case the GUI status copied was named ZSTANDARD and adjusted accordingly, adding and removing the desired buttons. A button was added called ‘%DELETE’.

3). Create the following report:
Code:
FORM user_command USING P_ucomm     LIKE sy-ucomm
                        P_selfield TYPE slis_selfield.

  case P_ucomm.
    when '&IC1'.
      "Verifica campo selecionado
      case p_selfield-sel_tab_field.
        when 'T_SAIDA-BELNR'.
          "Posiciona no registro selecionado
          read table t_saida index p_selfield-tabindex.
          "Move valores para o parâmetro ID de memória
          set parameter id 'BLN' field t_saida-belnr.
          set parameter id 'BUK' field t_saida-bukrs.
          "Chamar transação de exibição de docto contábil
          call transaction 'FB03' and skip first screen.
      endcase.
  endcase.

ENDFORM.  “User_command

As I’ve added an extra button to indicate which records should be deleted I need to identify a form to be called to process when this button is chosen.

Then when you call the ALV function you to specify the following extra details:

Code:
    call function ‘REUSE_ALV_GRID_DISPLAY’
         exporting  i_callback_program = gc_repid
                    I_CALLBACK_PF_STATUS_SET = ‘SET_PF_STATUS’
                    I_CALLBACK_USER_COMMAND  = ‘USER_COMMAND’
                    i_grid_title       = lc_grid_title
                    is_layout          = lc_layout
                    it_fieldcat        = gt_fieldcat
                    it_sort            = sort
                    i_save             = l_save
                    is_reprep_id       = l_bbs_id
                    is_variant         = l_variant
         tables     t_outtab           = %g00
         exceptions program_error      = 1
                    others             = 2.


***********************************

  call function 'REUSE_ALV_GRID_DISPLAY'
    exporting
      is_layout               = wa_layout
      i_callback_top_of_page  = 'TOP_PAGE'
      i_callback_program      = sy-repid
      i_save                  = 'A'
      i_callback_user_command = 'F_USER_COMMAND'
      it_fieldcat             = t_fieldcat
      it_events               = t_events[]
      it_sort                 = t_sort[]
    tables
      t_outtab                = t_saida.

form f_user_command using p_ucomm    like sy-ucomm
                          p_selfield type slis_selfield.

  case p_ucomm.
    when '&IC1'.
      "Verifica campo selecionado
      case p_selfield-sel_tab_field.
        when 'T_SAIDA-BELNR'.
          "Posiciona no registro selecionado
          read table t_saida index p_selfield-tabindex.
          "Move valores para o parâmetro ID de memória
          set parameter id 'BLN' field t_saida-belnr.
          set parameter id 'BUK' field t_saida-bukrs.
          "Chamar transação de exibição de docto contábil
          call transaction 'FB03' and skip first screen.
      endcase.
  endcase.

endform.

****************************************************************************

*&--------------------------------------------------------------------*
*&      Form  F_USER_COMMAND_ANUAL
*&--------------------------------------------------------------------*
form f_user_command3  using ucomm  like sy-ucomm 
                            selfield type slis_selfield.

  case ucomm.

    when '&AIRES'.

      message i011(pc) with text-x01.

    when others.

      if not selfield-tabindex = 0.
        read table ti_saida3 index selfield-tabindex.
        case selfield-fieldname.

          when 'BELNR2'.
            set parameter id 'BLN' field ti_saida3-belnr2.
            set parameter id 'BUK' field ti_saida3-bukrs.
            set parameter id 'GJR' field ti_saida3-gjahr.

            call transaction 'FB03' and skip first screen.

          when others.
        endcase.
      endif.

  endcase.

endform.                    "F_USER_COMMAND_MENSAL

****************************************************************************
TRANSAÇÕES

OB52 ---- manutenção de períodos contábeis.
OKP1 ---- períodos contábeis (Custos).
MMPV ---- Diferir períodos mm (mestre materiais).

OBMSG  ---- Alterar tipos de mensagens standards (por exemplo E para I).
            Classe de msg 8B (11,12,13) cgcs duplicados.
OBA5   ---- 

********************************************************
Checar se período contábil está em aberto / fechado.

  data: vl_gjahr type gjahr,
        vl_monat type monat,
        vl_poper type T001B-FRPE1.

  call function 'FI_PERIOD_DETERMINE'
    exporting
      i_budat        = vbrk-fkdat
      i_bukrs        = vbrk-bukrs
    importing
      e_gjahr        = vl_gjahr
      e_monat        = vl_monat
    exceptions
      fiscal_year    = 1
      period         = 2
      period_version = 3
      posting_period = 4
      special_period = 5
      version        = 6
      posting_date   = 7
      others         = 8.

  if sy-subrc = 0.
    move vl_monat to vl_poper.
    call function 'FI_PERIOD_CHECK'
      exporting
        i_bukrs          = vbrk-bukrs
        i_gjahr          = vl_gjahr
        i_koart          = '+'
        i_monat          = vl_poper
      exceptions
        error_period     = 1
        error_period_acc = 2
        others           = 3.

    if sy-subrc ne 0.
      message e000(zsd) with 'Período contábil fechado! Data estorno:' vbrk-fkdat.
    endif.
  endif.

****************************************************************************

    WRITE vc_number USING EDIT MASK '__.___.___/____-__' TO vc_cgc.

****************************************************************************
Alterar atributo de campos de um tablecontrol (table-control) numa tela:
data: tc_dados    type tableview using screen '0260',
      wa_cols     type cxtab_column.

    loop at tc_dados-cols into wa_cols.
      if wa_cols-screen-name = 'T_ZTRT003-MOTIVO'.
        move c_x to wa_cols-invisible.
      endif.

      if wa_cols-screen-name = 'T_ZTRT003-DESCRICAO'.
        move c_x to wa_cols-invisible.
      endif.

      modify tc_dados-cols from wa_cols index sy-tabix.
    endloop.

****************************************************************************
Rotina para colocar frames invisíveis, em tempo de execução (dinâmicos). Desabilitar campos. disabled.

Para listbox:
parameter p_frgot as listbox USER-COMMAND ucom visible length 23.


REPORT ZTST .

selection-screen begin of block b1 with frame.
      parameters: p_rg1 RADIOBUTTON GROUP PGR1 user-command ucom,
                  p_rg2 RADIOBUTTON GROUP PGR1.
selection-screen end of block b1.

selection-screen begin of block b2 with frame.     "Screen-name = %B%_F004  (standard)
      parameters: p_b2a(6) type c  MODIF ID SC2,    "screen-group1
                  p_b2b(6) type c  MODIF ID SC2,    "screen-group1
                  p_b2c(6) type c  MODIF ID SC2.    "screen-group1
selection-screen end of block b2.

selection-screen begin of block b3 with frame.     "Screen-name = %B%_F007  (standard)
      parameters: p_b3a(6) type c  MODIF ID SC3,   "screen-group1
                  p_b3b(6) type c  MODIF ID SC3.   "screen-group1
selection-screen end of block b3.

DATA AUX(20).

at selection-screen output.

   loop at screen.
      if  ( ( ( screen-name = '%B%_F004' ) or ( screen-group1 = 'SC2' ) )
               and ( p_rg2 = 'X' ) )
           or ( ( ( screen-name = '%B%_F009' ) or ( screen-group1 = 'SC3' ) )
               and ( p_rg1 = 'X' ) ).
         SCREEN-invisible = 1.
         SCREEN-active    = 0.
         MODIFY SCREEN.
      ENDIF.
   endloop.

***************************************************************************************
Smartform

  call function 'SSF_FUNCTION_MODULE_NAME'
    exporting
      formname           = cc_form
    importing
      fm_name            = lc_fname
    exceptions
      no_form            = 1
      no_function_module = 2
      others             = 3.

  if sy-subrc eq 0.
    wa_options-tddest  = nast-ldest.
    wa_options-tdnewid = cc_true.
    wa_options-tdimmed = nast-dimme.
*>Hypermarcas - Marcelino Fiuza (ECH-441891) - 23.05.2008 - Inicio
    wa_options-tdcopies = nast-anzal.
*>Hypermarcas - Marcelino Fiuza (ECH-441891) - 23.05.2008 - Fim

    call function lc_fname
      exporting
        output_options   = wa_options
        user_settings    = space
        vbeln            = nast-objky
        spras            = nast-spras
      exceptions
        formatting_error = 1
        internal_error   = 2
        send_error       = 3
        user_canceled    = 4
        others           = 5.

    if sy-subrc eq 0.
      return_code = 0.
    endif.
  endif.

-----------------------------------------------------
Gerar PDF a partir de um Smartforms.

*** Configuração dos parâmetros de impressão
  p_control-device    = 'PRINTER'. "Setar Impressora
  p_imp-tdnewid       = 'X'.       "Nova ordem SPOOL
  p_imp-tddelete      = ' '.       "Eliminar após
saída
  IF NOT p_arq IS INITIAL.
    p_control-no_dialog = 'X'.       "Habilitar caixa
de diálogo
    p_imp-tdimmed       = ' '.       "Saída imediata
  ELSE.
    p_control-no_dialog = ' '.       "Habilitar caixa
de diálogo
    p_imp-tdimmed       = 'X'.       "Saída imediata
  ENDIF.

...

  CALL FUNCTION v_func
    EXPORTING
      control_parameters = p_control
      user_settings      = space   
      output_options     = p_imp
      nome_mes           = v_nome_mes
    IMPORTING
      job_output_info    = wa_retorno
    TABLES
      cabec              = t_cabecalho
      mes                = t_mes.

  READ TABLE wa_retorno INDEX 1.
  READ TABLE wa_retorno-spoolids INDEX 1 INTO v_spool.

  CALL FUNCTION 'CONVERT_OTFSPOOLJOB_2_PDF'
    EXPORTING
      src_spoolid                = v_spool
   IMPORTING
     pdf_bytecount               = numbytes
   TABLES
     pdf                         = pdf
 EXCEPTIONS
   err_no_otf_spooljob            = 1
   err_no_spooljob                = 2
   err_no_permission              = 3
   err_conv_not_possible          = 4
   err_bad_dstdevice              = 5
   user_cancelled                 = 6
   err_spoolerror                 = 7
   err_temseerror                 = 8
   err_btcjob_open_failed         = 9
   err_btcjob_submit_failed       = 10
   err_btcjob_close_failed        = 11
   OTHERS                         = 12
            .
  IF sy-subrc <> 0.
    MESSAGE e078(m2) WITH
    'Erro ao gerar arquivo PDF'(e01).
  ENDIF.

  CALL FUNCTION 'DOWNLOAD'
    EXPORTING
      bin_filesize = numbytes
      filename     = v_file
      filetype     = 'BIN'
    IMPORTING
      act_filename = p_path
      filesize     = numbytes
*      cancel       = cancel
    TABLES
      data_tab     = pdf.


***************************************************************************************
PDF a partir de um nr de spool:
programa standard RSTXPDFT3.
Através da função CONVERT_ABAPSPOOLJOB_2_PDF.

BCS_EXAMPLE_8 ---- pega o spool, transforma em PDF e envia por email.
***************************************************************************************


Você terá que utilizar a tabela COBK e a BKPF para ir do documento de CO ao documento de FI. O Link é o seguinte:
 
COBK-BELNR = COEP-BELNR
COBK-KOKRS = COEP-KOKRS
 
BKPF-AWKEY = ( COBK-REFBN + COBK-AWORG )

BSEG-BUKRS = BKPF-BUKRS
BSEG-GJAHR = BKPF-GJAHR
BSEG-BELNR = BKPF-BELNR
BSEG-BUZEI = COEP-ZLENR


***************************************************************************************
Para a quebra da página vc utiliza o comando abaixo para o campo,
seu caso o MATNR, que vc quer efetuar a quebra:
 
  t_sort-group     = '*'.          " * Indica quebra de página

***************************************************************************************
Deletar registro da NAST: ISP_NAST_DELETE (preencher KAPPL = 'NF', OBJKY = nr doc fiscal (zeros á esquerda)
                          preencher T_NAST (todos os campos chave da NAST).

---------------
Reimpressão NF (visualização prévia) -----> RSNAST0D
Tela de seleção:
Aplicação                       NF
Chave de objeto                 0000030683
Tipo de mensagem
Meio de transmissão             1
Tipo de seleção                 2

Imprime NF:
SUBMIT RSNAST00 AND RETURN
 WITH S_KAPPL = 'NF'
 WITH S_OBJKY = J_1BDYDOC-DOCNUM.


***************************************************************************************
PROBLEMAS COM ESPAÇOS EM BRANCO NO FINAL DE UM ARQUIVO

Use a opção:
TRUNC_TRAILING_BLANKS_EOL       = ' '
na GUI_DOWNLOAD

***************************************************************************************
Cria o e-mail na sost e tudo mais, porem ela cria esse e-mail com o
STATUS de 'Ainda nenhuma entrada na fila' e não envia o e-mail, ele
fica parado.

Solução:
Após a chamada da função, vc deverá executar o programa RSCONN01 através do 
submit.
Veja o exemplo.

*--- Função para mandar e-mail.
  CALL FUNCTION 'SO_NEW_DOCUMENT_SEND_API1'
    EXPORTING
      DOCUMENT_DATA                  = DOC_DATA
      DOCUMENT_TYPE                  = 'RAW'
      PUT_IN_OUTBOX                   = 'X'
      COMMIT_WORK                      = 'X'
    IMPORTING
      NEW_OBJECT_ID                    = OBJECT_ID
    TABLES
      OBJECT_CONTENT                   = OBJCONT
      RECEIVERS                              = RECEIVER
    EXCEPTIONS
      TOO_MANY_RECEIVERS                  = 1
      DOCUMENT_NOT_SENT                 = 2
      DOCUMENT_TYPE_NOT_EXIST       = 3
      OPERATION_NO_AUTHORIZATION = 4
      PARAMETER_ERROR                      = 5
      X_ERROR                                     = 6
      ENQUEUE_ERROR                          = 7
      OTHERS                                      = 8.

  IF SY-SUBRC = 0.
*    SUBMIT RSCONN01 AND RETURN.
     submit rsconn01 with mode = 'INT'
                    with output = 'X'
                    and return.
  ENDIF.


***************************************************************************************
hotspot em ALV TREE

Registre os eventos e depois construa a lógica em método próprio.

* define the events which will be passed to the backend
  " node double click
  event-eventid = CL_GUI_COLUMN_TREE=>EVENTID_NODE_DOUBLE_CLICK. 
  event-appl_event = 'X'. " process PAI if event occurs
  append event to events.

  " link click
  event-eventid = CL_GUI_COLUMN_TREE=>EVENTID_LINK_CLICK.
  event-appl_event = 'X'. 
  append event to events.

  CALL METHOD G_TREE->SET_REGISTERED_EVENTS
    EXPORTING
      EVENTS = EVENTS
    EXCEPTIONS
      CNTL_ERROR                = 1
      CNTL_SYSTEM_ERROR         = 2 
      ILLEGAL_EVENT_COMBINATION = 3.
  IF SY-SUBRC <> 0.
    MESSAGE A000.
  ENDIF.
* assign event handlers in the application class to each desired event
  SET HANDLER G_APPLICATION->HANDLE_NODE_DOUBLE_CLICK FOR G_TREE. 
  SET HANDLER G_APPLICATION->HANDLE_LINK_CLICK FOR G_TREE.

***************************************************************************************
Botões/botão/botao/botoes   ---------> Na tela de seleção (report)
REPORT  zbotao.

TYPE-POOLS: icon.

TABLES: sscrfields.

DATA  : sc_functxt  TYPE smp_dyntxt.

DATA: w_msg(20).

SELECTION-SCREEN FUNCTION KEY 1.
SELECTION-SCREEN FUNCTION KEY 2.
SELECTION-SCREEN FUNCTION KEY 3.

PARAMETERS: pa_wlkun LIKE rf42b-idntd.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN PUSHBUTTON /2(07) but1 USER-COMMAND but4.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN PUSHBUTTON /2(25) but2 USER-COMMAND but5.

AT SELECTION-SCREEN.

  CASE sy-ucomm.

*...switch worklist on or off:
    WHEN 'FC01'.
      w_msg = 'botao 1 pressionado'.
    WHEN 'FC02'.
      w_msg = 'botao 2 pressionado'.
    WHEN 'FC03'.
      w_msg = 'botao 3 pressionado'.
    WHEN 'BUT4'.
      w_msg = 'botao 4 pressionado'.
    WHEN 'BUT5'.
      w_msg = 'botao 5 pressionado'.

  ENDCASE.

INITIALIZATION.
  sc_functxt-icon_id    =  ICON_WS_TRUCK.
  sc_functxt-quickinfo  =  'Botão com Icone e Texto'.
  sc_functxt-icon_text  =  'Botão 1'.
  sscrfields-functxt_01 =  sc_functxt.

  sc_functxt-icon_id    =  ICON_MESSAGE_INFORMATION_SMALL.
  sc_functxt-quickinfo  =  'Botão com Icone'.
  sc_functxt-icon_text  =  ''.
  sscrfields-functxt_02 =  sc_functxt.

  sscrfields-functxt_03 = 'Botao 3'.  " Botão com Texto

*** Pushbutton
  but1                  = 'Botão 4'.

  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      name   = icon_information
      text   = 'Botão 5'
      info   = 'Pushbutton'
    IMPORTING
      RESULT = but2
    EXCEPTIONS
      OTHERS = 0.


START-OF-SELECTION.

  WRITE:/1 w_msg.

***************************************************************************************
BAPI_SALESORDER_CHANGE

---- Incluindo itens numa ordem existente - Início
report  zaltera_ov message-id sabapdocu.

parameters: p_vbeln  like vbak-vbeln obligatory.   "Nr da ordem de venda

data: e_order_header_in  type bapisdh1,
      e_order_header_inx type bapisdh1x,
      t_return           type table of bapiret2,
      wa_return          type bapiret2,
      t_order_item_in    type  table of bapisditm  with header line,
      t_order_item_inx   type  table of bapisditmx with header line,
      t_schedule_lines   type table of bapischdl   with header line,
      t_schedule_linesx  type table of bapischdlx  with header line.

data: begin of t_pedidos occurs 0,
        vbeln  like vbap-vbeln,
        posnr  like vbap-posnr,
      end of t_pedidos.

start-of-selection.

  "Busca os itens do pedido informado na tela de seleção
  select vbeln posnr into table t_pedidos
       from vbap
         where vbeln = p_vbeln.

  if sy-subrc ne 0.
    message i888 with 'Pedido não cadastrado'.
    stop.
  endif.

  sort t_pedidos by vbeln posnr.

  "Posicionar no último item do pedido
  loop at t_pedidos.
  endloop.

  refresh: t_return, t_order_item_in, t_order_item_inx.

  t_order_item_in-itm_number = t_pedidos-posnr + 10.   "Soma 10 ao último item encontrado
  t_order_item_in-material      = '2270-0'.
  t_order_item_in-target_qty    = 1.    "qtd vendida
  append t_order_item_in.

  t_order_item_inx-itm_number = t_order_item_in-itm_number.
  t_order_item_inx-updateflag = 'I'. "Inserir item
  t_order_item_inX-material   = 'X'.
  t_order_item_inX-target_qty = 'X'.
  append t_order_item_inx.

  t_schedule_lines-itm_number  = t_order_item_in-itm_number.
  t_schedule_lines-req_qty     = 1.
  append t_schedule_lines.

  t_schedule_linesx-itm_number  = t_order_item_in-itm_number.
  t_schedule_linesx-req_qty     = 'X'.
  t_schedule_linesx-updateflag  = 'I'. "Inserir item de remessa
  append t_schedule_linesx.

  e_order_header_inx-updateflag = 'U'.

  call function 'BAPI_SALESORDER_CHANGE'
    exporting
      salesdocument    = p_vbeln
      order_header_in  = e_order_header_in
      order_header_inx = e_order_header_inx
    tables
      return           = t_return
      order_item_in    = t_order_item_in
      order_item_inx   = t_order_item_inx
      schedule_lines   = t_schedule_lines     "Div. de remessa
      schedule_linesx  = t_schedule_linesx.

*     Trata Retorno da bapi
  read table t_return into wa_return with key type   = 'E'.

  if sy-subrc ne 0.  "Se não retornou erro, commit no banco de dados
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = 'X'.
  else.
    call function 'BAPI_TRANSACTION_ROLLBACK'.
  endif.
---- Incluindo itens numa ordem existente - Fim



---- alterando conditions de uma ordem existente - Início

Utilize a estrutura do logic_switch:

  logic_switch-cond_handl = 'X'.

Com isso deve ser atualizada a condition existente.

 -------------------------------------------------- 
 data: salesdocument like BAPIVBELN-VBELN,
       ORDER_HEADER_INX like BAPISDH1X,
       RETURN like BAPIRET2 occurs 0 with header line,
       CONDITIONS_IN like BAPICOND occurs 0 with header line,
       CONDITIONS_INX like BAPICONDX occurs 0 with header line.

 CLEAR: SALESDOCUMENT, ORDER_HEADER_INX.
 REFRESH: RETURN, CONDITIONS_IN, CONDITIONS_INX.

 SALESDOCUMENT = '0000000362'.
 ORDER_HEADER_INX-UPDATEFLAG = 'U'.
 CONDITIONS_IN-ITM_NUMBER = '000010'.
 CONDITIONS_IN-COND_ST_NO = '950'.
 CONDITIONS_IN-COND_COUNT = '01'.
 CONDITIONS_IN-COND_TYPE = 'ZPIS'.
 CONDITIONS_IN-COND_VALUE = 777.
 CONDITIONS_IN-CURRENCY = 'BRL'.
 APPEND CONDITIONS_IN.

 CONDITIONS_INX-ITM_NUMBER = '000010'.
 CONDITIONS_INX-COND_ST_NO = '950'.
 CONDITIONS_INX-COND_COUNT = '01'.
 CONDITIONS_INX-COND_TYPE = 'ZPIS'.
 CONDITIONS_INX-UPDATEFLAG = 'U'.
 CONDITIONS_INX-COND_VALUE = 'X'.
 CONDITIONS_INX-CURRENCY = 'X'.
 CONDITIONS_INX-COND_UNIT = 'X'.
 APPEND CONDITIONS_INX.

 CALL FUNCTION 'BAPI_SALESORDER_CHANGE'
 EXPORTING
   SALESDOCUMENT = SALESDOCUMENT
 * ORDER_HEADER_IN =
   ORDER_HEADER_INX = ORDER_HEADER_INX
 * SIMULATION =
 * BEHAVE_WHEN_ERROR = ' '
 * INT_NUMBER_ASSIGNMENT = ' '
   LOGIC_SWITCH = logic_switch
 TABLES
   RETURN = RETURN
 * ORDER_ITEM_IN =
 * ORDER_ITEM_INX =
 * PARTNERS =
 * PARTNERCHANGES =
 * PARTNERADDRESSES =
 * ORDER_CFGS_REF =
 * ORDER_CFGS_INST =
 * ORDER_CFGS_PART_OF =
 * ORDER_CFGS_VALUE =
 * ORDER_CFGS_BLOB =
 * ORDER_CFGS_VK =
 * ORDER_CFGS_REFINST =
 * SCHEDULE_LINES =
 * SCHEDULE_LINESX =
 * ORDER_TEXT =
 * ORDER_KEYS =
   CONDITIONS_IN = CONDITIONS_IN
   CONDITIONS_INX = CONDITIONS_INX
 * EXTENSIONIN =
 .
---- alterando conditions de uma ordem existente - Fim

***************************************************************************************
POPUP_TO_DECIDE_LIST (popup com radiobuttons gerados por tabela interna)

Pop-up screens
POPUP_TO_CONFIRM_STEP – Cria um box de mensagem com confirmação 
 
POPUP_TO_CONFIRM_WITH_MESSAGE - Cria um box de mensagem com 
confirmação E 5 LINHAS
 
POPUP_TO_CONFIRM_WITH_VALUE – Cria Box de Dialogo com passagem de 
valores como parametro.
POPUP_TO_DECIDE – Cria box de dialogo com opções especificadas.
 
POPUP_TO_DECIDE_WITH_MESSAGE – Cria Box de dialogo com opções 
especificas
POPUP_TO_DISPLAY_TEXT – Cria Box de Selecão com 2 linhas e Botão
POPUP_TO_SELECT_MONTH – Cria um Box de Seleção para Mês e Ano

DD_POPUP_WITH_INFOTEXT -- Popup mostra tabela interna.

POPUP_WITH_TABLE_DISPLAY_OK
POPUP_WITH_TABLE_DISPLAY – Cria um Box de seleção, a partir de uma 
tabela passada como parâmetro e retorna qual a opção selecionada 
pelo usuário.
 
POPUP_TO_CONFIRM – Exibe uma caixa de textos para confirmação
POPUP_TO_INFORM – Cria caixa de informação
POPUP_TO_CONFIRM_LOSS_OF_DATA – Box de Dialogo

POPUP_GET_VALUES   (Tela com campo editável)
POPUP_GET_VALUES_USER_HELP (Tela com campo editável e search help)

REUSE_ALV_POPUP_TO_SELECT

TH_POPUP – Exibe uma mensagem para o usuário Especificado, em 
qualquer client. (envia mensagem).

FITP_EDIT_TEXT  --- Popup para editar um texto, e depois retornar o texto.
        mode       = 'C'    "editar textos
        mode       = 'Q'    "exibir textos
        mode       = 'D'

------------------------------------------------------------------
include <icon>.

data: v_icone(6)  type c.

field-symbols: <icone> like icon_checked.

  DATA ti_lines TYPE STANDARD TABLE OF popuptext WITH HEADER LINE.
  ti_lines-text = 'Neste Popup, é possível colocar várias linhas.'.
  APPEND ti_lines.

  concatenate 'Docto miro:' wa_dados-miro 'Ano:' wa_dados-ano_miro 'informado, não existe.'
              'Danfe:' wa_dados-danfe into t_linetab-text separated by space.
  perform f_grava_log using 'E'.

  CALL FUNCTION 'DD_POPUP_WITH_INFOTEXT'
       EXPORTING
            titel        = 'Título do Popup'
            start_column = 30
            start_row    = 6
            end_column   = 80
            end_row      = 12
       TABLES
            lines        = ti_lines.


form f_grava_log using p_status.

  if p_status = 'S'.       "registro regravado com sucesso
    assign icon_green_light to <icone>.
  elseif p_status = 'W'.   "registro não regravado
    assign icon_yellow_light to <icone>.
  else.                "registro não regravado
    assign icon_red_light to <icone>.
  endif.

  v_icone = <icone>.
  concatenate v_icone t_linetab-text into t_linetab-text separated by space.
  append t_linetab.

endform.

***************************************************************************************
Desabilitar Botão de Totalização do ALV


Segue:

IT_EXCL_BUTT type SLIS_T_EXTAB.
wa_excl_buttons type SLIS_EXTAB

wa_excl_buttons-fcode = &OUP
append wa_excl_buttons to it_excl_butt.  "SORT ASC
wa_excl_buttons-fcode = &ODN
append wa_excl_buttons to it_excl_butt. "SORT DES 
wa_excl_buttons-fcode = &ILT
append wa_excl_buttons to it_excl_butt. "FILTER
wa_excl_buttons-fcode = &UMC
append wa_excl_buttons to it_excl_butt. "TOTAL

***************************************************************************************
FUNCTION DATE_COMPUTE_DAY.

Retorna valor da data em dia.
Ex: 08/01/2007 (segunda-feira)
Retorna o valor 1.

***************************************************************************************
Limpar parameter id.

    set parameter id 'AVK' field space.

Tabela pra criar (SM30) parameter ID: TPARA.
Tabela que contém o id preenchido: USR05 (dados própios).


***************************************************************************************
Chamar uma tela de diálogo (SM30), sem criar uma transação para a mesma:

Obs: Pra deixar somente leitura:
  - Passar 'S' para o ACTION;
  - Passar a tabela EXCL_CUA_FUNCT, preenchida como a seguir:
    MOVE 'AEND' TO FU_TO_EXCL-FUNCTION.  "Excluir o botão 'modificar'.
    APPEND FU_TO_EXCL.

DATA: BEGIN OF FU_TO_EXCL OCCURS 10.
        INCLUDE STRUCTURE VIMEXCLFUN.
DATA: END OF FU_TO_EXCL.

  call function 'VIEW_MAINTENANCE_CALL'
    exporting
      action                       = 'U'             "U - update       S - Show
      view_name                    = 'ZFIT156'
*    tables
*      EXCL_CUA_FUNCT               = FU_TO_EXCL
    exceptions
      client_reference             = 1
      foreign_lock                 = 2
      invalid_action               = 3
      no_clientindependent_auth    = 4
      no_database_function         = 5
      no_editor_function           = 6
      no_show_auth                 = 7
      no_tvdir_entry               = 8
      no_upd_auth                  = 9
      only_show_allowed            = 10
      system_failure               = 11
      unknown_field_in_dba_sellist = 12
      view_not_found               = 13
      maintenance_prohibited       = 14
      others                       = 15.

  if sy-subrc <> 0.
    message id sy-msgid type sy-msgty number sy-msgno
            with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

***************************************************************************************
após executar normalmente um report, retorna à tela de parâmetros com todos os campos inicializados
(em branco/zero).

No final do processamento coloque o seguinte código:
LEAVE TO TRANSACTION SY-TCODE.

Ou no final da execução coloque “FREE MEMORY”.

***************************************************************************************
Executar uma Store Procedure Oracle diretamente do ABAP.

 EXEC sql.                                                                  
  EXECUTE PROCEDURE lf.p_zylfr005_72@lf ( IN :v_id_proc, IN :v_tp_rel)      
 ENDEXEC.              

***************************************************************************************

Define uma coluna no Layout, para checkbox. ALV Não Orientado Objeto.

  gs_layout-box_fieldname     = 'XSELP'.

  clear wa_fieldcat.
  wa_fieldcat-fieldname = 'XSELP'.
  wa_fieldcat-tech      = 'X'.
  append wa_fieldcat to gt_fieldcat.

---------------
Para ícones.
  clear wa_fieldcat.
  wa_fieldcat-fieldname = 'ICO_AUGP'.
  wa_fieldcat-icon      = 'X'.
  append wa_fieldcat to gt_fieldcat.


***************************************************************************************
Traduções elementos de texto - como transportar?

Se estiver em versão 4.7,   utilize pela SE38, o report RS_LXE_RECORD_TORDER


***************************************************************************************
        CALL FUNCTION 'ZMMF001' IN BACKGROUND TASK AS SEPARATE UNIT

***************************************************************************************
  clear:  v_seq, v_end.

  import v_seq from MEMORY id 'ZCOF014_SEQ'.
  add 1 to v_seq.
  export v_seq to MEMORY id 'ZCOF014_SEQ'.
  condense v_seq NO-GAPS.

  CALL FUNCTION 'ZCOF014'
    STARTING NEW TASK v_seq
    PERFORMING f_task_end ON END OF TASK
    EXPORTING
      i_kalnr     = p_kalnr
      i_bdatj     = lh_bdatj
      i_poper     = lh_poper
      i_untper    = lh_untper
      i_curtp     = lh_curtp
      i_nivel_aux = v_nivel
      i_mlkey     = mlkey
      i_quantity  = lh_quantity
    TABLES
      tab_saida   = t_saida_aux.

  "Espera que a função seja finalizada. A variável v_flag é setada como 'X', qdo a função é finalizada.
  wait until v_end = 'X'.

FORM f_task_end USING u_taskname.

  "Recebe o resultado da função, após a sua execução em outra task
  RECEIVE RESULTS FROM FUNCTION 'ZCOF014'
      TABLES
        tab_saida   = t_saida_task.

  t_saida_aux[] = t_saida_task[].
  v_end = 'X'.

ENDFORM.                    "f_task_end

***************************************************************************************
Dispara um refresh numa tela ALV.

Detalhe para o comando: SET USER-COMMAND '&NTE'. " Refresh


REPORT z_alv_auto_refresh.
* The list is auto-refreshed (5 seconds) *


TYPE-POOLS: slis. " ALV Global Types

DATA :
gt_user LIKE uinfo OCCURS 0 WITH HEADER LINE. " User info in SM04

START-OF-SELECTION.

PERFORM f_read_data.

PERFORM f_display_data.


FORM f_read_data.

REFRESH gt_user.

* Get User's info
CALL FUNCTION 'THUSRINFO'
TABLES
usr_tabl = gt_user.

* Wait in a task
PERFORM f_call_rfc_wait.

ENDFORM. " F_READ_DATA

FORM f_display_data.

DEFINE m_sort.
add 1 to ls_sort-spos.
ls_sort-fieldname = &1.
append ls_sort to lt_sort.
END-OF-DEFINITION.

DEFINE m_event_exit.
clear ls_event_exit.
ls_event_exit-ucomm = &1.
ls_event_exit-after = 'X'.
append ls_event_exit to lt_event_exit.
END-OF-DEFINITION.

DATA :
ls_layout TYPE slis_layout_alv,
lt_sort TYPE slis_t_sortinfo_alv,
ls_sort TYPE slis_sortinfo_alv,
lt_event_exit TYPE slis_t_event_exit,
ls_event_exit TYPE slis_event_exit.

* Build Sort Table
m_sort 'ZEIT'.

* Build Event Exit Table
m_event_exit '&NTE'. " Refresh

ls_layout-zebra = 'X'.
ls_layout-colwidth_optimize = 'X'.

CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
EXPORTING
i_callback_program = sy-cprog
i_callback_user_command = 'USER_COMMAND'
is_layout = ls_layout
i_structure_name = 'UINFO'
it_sort = lt_sort
it_event_exit = lt_event_exit
TABLES
t_outtab = gt_user.

ENDFORM. " F_DISPLAY_DATA

FORM user_command USING i_ucomm TYPE syucomm
is_selfield TYPE slis_selfield. "#EC CALLED

CASE i_ucomm.
WHEN '&NTE'.
PERFORM f_read_data.
is_selfield-refresh = 'X'.
SET USER-COMMAND '&OPT'. " Optimize columns width
ENDCASE.

ENDFORM. " USER_COMMAND

FORM f_call_rfc_wait.

DATA lv_mssg(80). "#EC NEEDED

* Wait in a task
CALL FUNCTION 'RFC_PING_AND_WAIT' STARTING NEW TASK '001'
PERFORMING f_task_end ON END OF TASK
EXPORTING
seconds = 5 " Refresh time
busy_waiting = space
EXCEPTIONS
RESOURCE_FAILURE = 1
communication_failure = 2 MESSAGE lv_mssg
system_failure = 3 MESSAGE lv_mssg
OTHERS = 4.

ENDFORM. " F_CALL_RFC_WAIT

FORM f_task_end USING u_taskname.

DATA lv_mssg(80). "#EC NEEDED

* Receiving task results
RECEIVE RESULTS FROM FUNCTION 'RFC_PING_AND_WAIT'
EXCEPTIONS
RESOURCE_FAILURE = 1
communication_failure = 2 MESSAGE lv_mssg
system_failure = 3 MESSAGE lv_mssg
OTHERS = 4.

CHECK sy-subrc EQ 0.
SET USER-COMMAND '&NTE'. " Refresh

ENDFORM. " F_TASK_END

******************************************************************************

Arrendondamento.

REPORT z_teste .

DATA: x(13) TYPE p DECIMALS 6,
      y(13) TYPE n,
      z(13) TYPE p DECIMALS 2.

DATA: str(13) TYPE c.

x = '2356.345667'.

y = x * 1000000.

IF y+9(1) >= '5'.
  z = x - '0.01'.
ELSE.
  z = x.
ENDIF.

WRITE: x, y, z.

******************************************************************************
Atualiza banco de dados externo.

EXEC SQL.
  UPDATE ORESAP
     SET [Data Evento sap] = :W_DATUM
                  WHERE ID = :T_DATI-ID
                   AND CID = :T_DATI-PERNR
ENDEXEC.
COMMIT WORK.


******************************************************************************
Insere em banco de dados externo.

TRY.

  EXEC SQL.

     INSERT INTO SC_T_NOTA_FISCAL (ORG_VENDA,DATA_LANCAMENTO,NUM_DOCUMENTO,COD_CLIENTE,
           NOME_CLIENTE,COD_VENDEDOR,NOME_VENDEDOR,VALOR_MONTANTE)
       VALUES (:T_NOTA_FISCAL-ORG_VENDA,:T_NOTA_FISCAL-DATA_LANCAMENTO,
               :T_NOTA_FISCAL-NUM_DOCUMENTO,:T_NOTA_FISCAL-COD_CLIENTE,
               :T_NOTA_FISCAL-NOME_CLIENTE,:T_NOTA_FISCAL-COD_VENDEDOR,
               :T_NOTA_FISCAL-VALOR_MONTANTE)

  ENDEXEC.

CATCH cx_sy_native_sql_error INTO exc_ref.

     error_text = exc_ref->get_text( ).

ENDTRY.

******************************************************************************
Refresh REUSE_ALV_LIST_DISPLAY.

FORM user_command  USING r_ucomm LIKE sy-ucomm 
                         rs_selfield TYPE slis_selfield. 

* Aufruf der Transaktion 
  CALL TRANSACTION 'ABC'. 
  
* Daten neu einlesen 
  .... 

* dem ALV zum refresh bewegen 
  rs_selfield-refresh = 'X'. 

ENDFORM. 


******************************************************************************
Não arredondar valores

DATA: gvl1 TYPE p DECIMALS 6,

      gvl2 TYPE p DECIMALS 2.

 

gvl1 = '2356.345667'.  " Vlr. com todas as decimais:        2.356,345667

gvl1 = ( gvl1 * 100 ). " Vlr. x 100:                      235.634,566700

gvl2 = trunc( gvl1 ).  " Vlr. truncado (sem decimais):    235.634,00

gvl2 = ( gvl2 / 100 ). " Vlr. ÷ por 100 com duas decimais:  2.356,34

WRITE: / gvl1,

         gvl2.

----------------
REPORT  ZTSTX.

PARAMETERS v_qtd type p decimals 3.

Data: v_inteiro(8)     type c,
      v_decimal     type p decimals 3.

v_inteiro = trunc( v_qtd ).
v_decimal = frac( v_qtd ).

if v_decimal = 0.
   write: /01 'O Valor é inteiro'.
endif.

write: /01 'Vr inteiros: ',  v_inteiro.
write: /01 'Vr decimal: ', v_decimal.
----------------------------------

Separa valores inteiros e decimal de um campo.

REPORT  ZTSTX.

Data: v_qtd type goitem-anzgeb value '1.600',
      v_inteiro(5)     type c,
      v_decimal(5)     type c,
      v_arredonda(5)   type n.

v_inteiro = trunc( v_qtd ).
v_decimal = frac( v_qtd ).
v_arredonda = v_qtd.

write: 'Vr inteiros: ',  v_inteiro.
write: /01 'Vr decimal: ', v_decimal.
write: /01 'Vr arredondado pra cima: ',v_arredonda.

-----------------------------------------------------------------------------------------------------------
report  ztestey.

parameters: p_netpr(14) type c.

data: v_str(16)     type c,
      v_inteiro(10) type c,
      v_decimal(06) type c,
      v_kpein       type kpein,
      v_strlen      type i,
      vr_aux(16)    type c.

start-of-selection.

  v_str = p_netpr.
  "Converte o campo valor...
  replace all occurrences of '.' in v_str with ' '.
  replace ',' with '.' into v_str.

  clear: v_inteiro, v_decimal, v_kpein.
  split v_str at '.' into v_inteiro v_decimal.

  "Verifica quantas casas decimais tem o campo...
  v_strlen = strlen( v_decimal ).
  v_kpein  = 1.

  while v_strlen > 2.
    "Realizar cálculos para encontrar valor e unidade POR
    perform f_multiplica_valor.
    v_strlen = strlen( v_decimal ).
  ENDWHILE.

  write:/01 vr_aux,
        /01 v_kpein.


*&---------------------------------------------------------------------*
*&      Form  f_multiplica_valor
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_PEIN     text
*----------------------------------------------------------------------*
*form f_multiplica_valor using p_kpein.
form f_multiplica_valor.

  v_kpein = v_kpein * 10.
  vr_aux = v_str * v_kpein.
  condense vr_aux no-gaps.
  "Verifica quantas casas decimais tem o campo...
  clear: v_inteiro, v_decimal.
  split vr_aux at '.' into v_inteiro v_decimal.

endform.                    "f_multiplica_valor

-----------------------------------------------------------------------------------------------------------

        LC_VRICMS    TYPE P DECIMALS 4,
        lc_char(18)    type c,
        lc_val_inteiro(14)  type c,
        lc_val_decimal(04)  type c,
        lc_montante_char(18) type c,
        LC_MONTANTE_IMPOSTO LIKE BSID-WRBTR,

             MOVE LC_VRICMS TO LC_CHAR.
*          Separa os valores inteiro e decimal
             SPLIT lc_char AT '.' INTO lc_val_inteiro
                                       lc_val_decimal.
*          Alinha o valor inteiro a direita
             write lc_val_inteiro to lc_val_inteiro right-justified.
*          Monta o vr inteiro e decimal na variável montante(caracter)
             concatenate lc_val_inteiro '.' lc_val_decimal(02)
                                            into lc_montante_char.
*          Move o valor montado para a variável(valor)
             lc_montante_imposto = lc_montante_char.

Outra forma:   TRUNC
         Lc_val_inteiro = trunc( lc_montante_imposto ).

************************************************************************
Formatação color linha na impressão.

     FORMAT COLOR COL_BACKGROUND.
     FORMAT INTENSIFIED ON.
     FORMAT INVERSE OFF.

     FORMAT RESET.   "Normal

     format color col_total.




  write : 'NORMAL'.

  format intensified off.     "Linha em negrito

  write : 'NEGRITO '.

  format color 1.          "Linha fundo azul

  write : 'FUNDO AZUL'.

  format color 3.          "Linha fundo amarelo

  write : 'FUNDO AMARELO'.

* Normaliza fonte
  format color off.
  format intensified on.

  write : 'NORMAL'.


************************************************************************
Mudar a Classe de Desenvolvimento de "Z" para $TMP.

Programa RSWBO052.
 
************************************************************************

Ordem de Venda em aberto.

tabelas vbuk and vbup.

************************************************************************
Localizar BTEs.

Transação FIBF

Colocar um ponto de parada dentro da função abaixo.

      CALL FUNCTION 'BF_FUNCTIONS_READ'                   
           EXPORTING                                      
                I_EVENT       = I_EVENT                   
                I_LAND        = LAND                      
                I_APPLK       = I_APPLK                   
           IMPORTING                                      
                E_EMPTY = EMPTY                           
           TABLES                                         
                T_FMSAP = FMSAPTAB                        
                T_FMPRT = FMPRTTAB                        
                T_FMCUS = FMCUSTAB.                       
      IF EMPTY = 'X'.                                     
        MESSAGE S015 WITH I_EVENT RAISING NOTHING_FOUND.  
      ENDIF.                                              

************************************************************************
Excluir ícones em tela de seleção.

Crie uma tabela interna nas declarações globais do programa como esta abaixo:

 
* Itab para salvar os objetos do status “%_00” que serão ocultos
DATA ti_exclude TYPE STANDARD TABLE OF rsexfcode WITH HEADER LINE.
 
No início do evento INITIALIZATION, copie e cole as linhas abaixo.
E você não precisará mudar nada nestas linhas, exceto o parâmetro p_program da função RS_SET_SELSCREEN_STATUS, caso o nome da sua cópia não seja ZRFBELJ10.

CLEAR ti_exclude.
FREE ti_exclude.
* O Botão “DYNS” é o botão de seleção dinâmica e deverá ser oculto
MOVE 'DYNS' TO ti_exclude-fcode.
APPEND ti_exclude.

* Função para chamar o PF-STATUS '%_00' e ocultar o botão “DYNS”
CALL FUNCTION 'RS_SET_SELSCREEN_STATUS'
     EXPORTING
          p_status  = '%_00'
          p_program = 'ZRFBELJ10'
     TABLES
          p_exclude = ti_exclude.

************************************************************************
BAPI_SALESORDER_CHANGE - para criar um novo item em uma ordem já existente.

Na SCHEDULE_LINESX você marca com um X os valores que você está modificando na SCHEDULE_LINES.

************************************************************************
Module Pool - Desabilitação de campo.

- Crie no seu layout de tela os radios-buttons “RB_HAB” e “RB_DES”.

- Não esqueça de, em ambos os radios-buttons, na tela de atributos dos botões criados, em “Cód.função”, colocar “RB_1”.

- No seu include TOP, declare os radios-buttons criados com tamanho 1, type c:

DATA: rb_hab, " Botão que habilita campo
      rb_des. " Botão que desabilita campo

- No PBO da lógica de processamento de sua tela, chame (se ainda não chamada com finalidades afins) o módulo “MODIF_ATRIB_TELA”:

PROCESS BEFORE OUTPUT.

 MODULE modif_atrib_tela.

- No seu include “SAPMZ...O01”, crie (se ainda não existir um com finalidades afins) o módulo “MODIF_ATRIB_TELA” codificando-o assim (digamos que o campo que você queira desabilitar seja o campo “T001-BUKRS” [Empresa]):

MODULE modif_atrib_tela OUTPUT.

  CASE sy-ucomm.

    WHEN 'RB_1'.

      LOOP AT SCREEN.

        CHECK screen-name EQ 'T001-BUKRS'.

        CASE 'X'.

          WHEN rb_hab.

            screen-input = '1'.

          WHEN rb_des.

            screen-input = '0'.

        ENDCASE.

        MODIFY SCREEN.

      ENDLOOP.

  ENDCASE.

ENDMODULE.  

******************************************************************************************
Use o programa RSBDCSUB para programar as pastas (Job na Transação SM35).

******************************************************************************************

Gravar arquivo TXT (Tabulado) no Unix.

DATA:

      vl_tabx TYPE xstring,                       "tabulação hexabinaria

      vl_tab TYPE string,                         "tabulação em caracter

      vl_conv      TYPE REF TO cl_abap_conv_in_ce,  "para transformar tab

      vl_len       TYPE i,  "para transformar tab


* declara vl_tabx com o valor do tab em Hexadecimal

  vl_tabx = '09'.

* declara vl_tab com o valor do tab em caracter

  vl_conv = cl_abap_conv_in_ce=>create( input = vl_tabx ).

  vl_conv->read( IMPORTING data = vl_tab len = vl_len ).

 

R30;R30; outros codigosR30;.

loop

      CONCATENATE campo1 campo2 R30;

                  INTO vl_campo SEPARATED BY vl_tab.


      TRANSFER vl_campo TO vl_caminho.

endloop

******************************************************************************************
SE78 --- Importar e salvar imagem (picture) pra usar em sapscript.

No sapscript:
BITMAP Z_HYPM_LOGO OBJECT GRAPHICS ID BMAP TYPE BCOL DPI 200
******************************************************************************************
Para alv vc tem que carregar o logo pela transação OAER (PICTURES E OT)...
depois vc carrega no seu programa o logo...
 
*&---------------------------------------------------------------------*
*&     FORM  TOP_OF_PAGE                                               *
*&---------------------------------------------------------------------* 
form top_of_page.
  call function 'REUSE_ALV_COMMENTARY_WRITE'
    exporting
      i_logo             = 'LOGOAIRES'  "ENJOYSAP_LOGO
      it_list_commentary = gt_list_top_of_page.

endform.                               " top_of_page


******************************************************************************************
Precisa estar configurado o STMP, na transação 'SCOT'.

form z_envio_nova_senha tables t_return structure bapiret2
                         using new_pass
*                              utilizador
                               username
                               e_mail.

  data document_data    type sodocchgi1.
  data packing_list     type table of sopcklsti1.
  data packlist_wa      type sopcklsti1.
  data : begin of tab_receiver occurs 0.
          include structure somlreci1.
  data : end of tab_receiver.
  data contents_txt     type soli_tab.
  data: linha(255).
  data: remetente  like  soextreci1-receiver.

  document_data-obj_descr = 'XXXX - senha/associado'.
* remetente = 'XXXX'.

  packlist_wa-body_start = 1.
  packlist_wa-body_num   = 255.
  packlist_wa-doc_type   = 'RAW'.
* packlist_wa-obj_descr  = 'Texto do mail'.
  append packlist_wa to packing_list.

  tab_receiver-rec_date  = sy-datum.
  tab_receiver-rec_type  = 'U'.
  tab_receiver-receiver  = e_mail.
  tab_receiver-express   = 'X'.
* tab_receiver-rec_id    = 'XXXX'.
  tab_receiver-no_forward = 'X'.
  append tab_receiver.

  clear linha.
  concatenate 'senha: ' new_pass into linha.
  append linha to contents_txt.
  clear linha.
  concatenate 'utilizador: ' username into linha.
  append linha to contents_txt.

  call function 'SO_DOCUMENT_SEND_API1'
    exporting
      document_data              = document_data
*     put_in_outbox              = 'X'
      commit_work                = 'X'
*     sender_address             = remetente
*   importing
*     sent_to_all                = sent_to_all
    tables
      packing_list               = packing_list
      contents_txt               = contents_txt
*     contents_hex               = contents_hex
      receivers                  = tab_receiver
    exceptions
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      others                     = 8.

  if sy-subrc = 0.
*   SAPconnect: iniciar processo de envio
    submit rsconn01 and return.
  else.
    raise no_email.
  endif.

endform.                    " z_envio_nova_senha


******************************************************************************************
Lê uma stored procedure de um banco externo, e grava numa tabela interna.

  EXEC SQL PERFORMING LOOP_OUTPUT.
    SELECT OPER_IN_TI_LOC_REF, ORGA_CD_CBI,        PROD_CD_ID,
           TO_CHAR(DMET_DT_REFERENCIA, 'DD/MM/YYYY'),
           OPLO_CD_ID,         DMET_IN_ENT_SAI,
           METR_CD_ID,         OPER_IN_TI_LOC_O_D, DMET_CD_LOCAL,
           DMET_SG_LOCAL,      DMET_QN_VOL_AMB,    DMET_QN_VOL_A20,
           DMET_QN_MASSA
      INTO :WA_TABELA-OPER_IN_TI_LOC_REF, :WA_TABELA-ORGA_CD_CBI,
           :WA_TABELA-PROD_CD_ID,         :WA_TABELA-DMET_DT_REFERENCIA,
           :WA_TABELA-OPLO_CD_ID,         :WA_TABELA-DMET_IN_ENT_SAI,
           :WA_TABELA-METR_CD_ID,         :WA_TABELA-OPER_IN_TI_LOC_O_D,
           :WA_TABELA-DMET_CD_LOCAL,      :WA_TABELA-DMET_SG_LOCAL,
           :WA_TABELA-DMET_QN_VOL_AMB,    :WA_TABELA-DMET_QN_VOL_A20,
           :WA_TABELA-DMET_QN_MASSA
      FROM ZTBSD0007
      WHERE ORGA_CD_CBI                             = :P_ORGAO      AND
            TO_CHAR(DMET_DT_REFERENCIA,'YYYYMMDD') >= :S_PERIOD-LOW AND
            TO_CHAR(DMET_DT_REFERENCIA,'YYYYMMDD') <= :S_PERIOD-HIGH
  ENDEXEC.



FORM loop_output.

  DATA vl_qtde(16) TYPE p DECIMALS 3.

  MOVE wa_tabela-oper_in_ti_loc_ref TO tg_tabela-oper_in_ti_loc_ref.
  MOVE wa_tabela-orga_cd_cbi        TO tg_tabela-orga_cd_cbi.
  MOVE wa_tabela-prod_cd_id         TO tg_tabela-prod_cd_id.
  MOVE wa_tabela-dmet_dt_referencia TO tg_tabela-dmet_dt_referencia.
  MOVE wa_tabela-oplo_cd_id         TO tg_tabela-oplo_cd_id.
  MOVE wa_tabela-dmet_in_ent_sai    TO tg_tabela-dmet_in_ent_sai.
  MOVE wa_tabela-metr_cd_id         TO tg_tabela-metr_cd_id.
  MOVE wa_tabela-oper_in_ti_loc_o_d TO tg_tabela-oper_in_ti_loc_o_d.
  MOVE wa_tabela-dmet_cd_local      TO tg_tabela-dmet_cd_local.
  MOVE wa_tabela-dmet_sg_local      TO tg_tabela-dmet_sg_local.

  vl_qtde = wa_tabela-dmet_qn_vol_amb / 1000.
  MOVE vl_qtde TO tg_tabela-dmet_qn_vol_amb.

  vl_qtde = wa_tabela-dmet_qn_vol_a20 / 1000.
  MOVE vl_qtde TO tg_tabela-dmet_qn_vol_a20.

  vl_qtde = wa_tabela-dmet_qn_massa / 1000.
  MOVE vl_qtde TO tg_tabela-dmet_qn_massa.

  APPEND tg_tabela.
  CLEAR  tg_tabela.

ENDFORM.                    " LOOP_OUTPUT

******************************************************************************************
match code diretório/arquivo.

data: w_file       like rlgrap-filename.

selection-screen begin of block b3.
parameters: p_filen(250) type c default 'C:\R3C0200_NEW.TXT'.
selection-screen end  of block b3.

at selection-screen on value-request for p_filen.

  call function 'WS_FILENAME_GET'
       exporting
            def_filename = ' '
            def_path     = 'C:\'
            mask         = ',Textos,*.txt,Todos,*.*.'
            mode         = 'O'
            title        = text-l34
       importing
            filename     = w_file
       exceptions
            others.

  if sy-subrc =  0.
    p_filen = w_file.
  endif.

*************************************************************************************
você consegue resolver o problema com FIELD-SYMBOLS, segue abaixo como seria a rotina para carregamento dinâmico da tabela interna:
 
DATA: BEGIN OF tab_dados_texto OCCURS 0,
           filler(80) TYPE c,
      END OF tab_dados_texto,
 
      BEGIN OF tab_campos OCCURS 0,
           campo(10)             TYPE c,
           posicao_inicial(02)   TYPE n,
           tamanho(02)           TYPE n,
      END OF tab_campos,
 
      BEGIN OF tab_interna OCCURS 0,
          mandt(3) TYPE c,
          codigo(04) TYPE c,
          nome(05) TYPE c,
          idade(02) TYPE c,
      END OF tab_interna.
 
FORM inclui_dados TABLES 
                                         p_tab_dados_texto STRUCTURE tab_dados_texto
                                         p_tab_campos STRUCTURE tab_campos
                                         p_tab_interna.
 
  FIELD-SYMBOLS: <table>, <field>.
 
  DATA: i_pos   TYPE i,
             i_len   TYPE i.
 
  ASSIGN p_tab_interna TO <table>.
 
  LOOP AT tab_dados_texto.
 
    CLEAR p_tab_interna.
 
    LOOP AT tab_campos.
 
      i_pos = p_tab_campos-posicao_inicial.
      i_len = p_tab_campos-tamanho.
 
      ASSIGN COMPONENT p_tab_campos-campo
        OF STRUCTURE <table> TO <field>.
      <field> = p_tab_dados_texto+i_pos(i_len).
 
    ENDLOOP.
 
    APPEND p_tab_interna.
 
  ENDLOOP.
 
ENDFORM.                    " inclui_dados


**********************************************************************
Relatório Write em spool.

Abre spool -----
  new-page print on
     destination ''
     immediately 'X'
     list name 'Spool'
     cover text text-001
     keep in spool 'X'
     no dialog
     line-size 110.

imprime: 
    write: / 'xxxxxx'
       ...
    write: / 'yyyyyy'

Close spool -----
  new-page print off.


------------------------------------------------
Abrindo tela diálogo impressão (write):

    new-page print on
      destination saida            "nome impressora
      copies c_copias              "nr cópias
      immediately v_flag_imed      "X --- impressão imediata
      keep in spool v_flag_keep    "X
      dataset expiration c_dias.   "0 ---- No. de dias no spool
*  NO DIALOG.

**********************************************************************
Provavelmente a transação que vc está chamando possue um commit work dentro dela, por isso o processo é cancelado.
Para prevenir isso, execute o BI passando o parâmetro RACOMMIT da estutura CTU_PARAMS preenchido. Isso força a continuação do BI

Mesmo que haja um commit.. Ex.:

DATA: BEGIN OF T_OPT,
          DISMODE  TYPE CTU_PARAMS-DISMODE,
          UPDMODE  TYPE CTU_PARAMS-UPDMODE,
          CATTMODE TYPE CTU_PARAMS-CATTMODE,
          DEFSIZE  TYPE CTU_PARAMS-DEFSIZE,
          RACOMMIT TYPE CTU_PARAMS-RACOMMIT,
          NOBINPT  TYPE CTU_PARAMS-NOBINPT,
          NOBIEND  TYPE CTU_PARAMS-NOBIEND,
        END OF T_OPT.

  T_OPT-DISMODE = C_MODO.
  T_OPT-UPDMODE = C_UPDATE.
  T_OPT-RACOMMIT = 'X'.

CALL TRANSACTION 'TRANSAÇAO' USING BDCDATA
                       OPTIONS FROM T_OPT
                 MESSAGES INTO MESSTAB.

**********************************************************************
O código abaixo serve para dar permissão total no arquivo gerado no UNIX.

* Permissão para acessar o arquivo

DATA: VG_COMANDO(1000) TYPE C.
DATA: TI_TABELA(2000) OCCURS 0 WITH HEADER LINE.

CLEAR VG_COMANDO.

CONCATENATE 'chmod 777' P_PATH1 P_PATH1 INTO VG_COMANDO SEPARATED BY SPACE.

CALL 'SYSTEM' ID 'COMMAND' FIELD VG_COMANDO
              ID 'TAB'     FIELD TI_TABELA-*SYS*.

**********************************************************************
Encriptação de senha no banco.


  data: l_senha_banco like zmmminuta03-senha,
        l_senha_encriptada type dbcon_pwd,    "Senha encriptada
        l_senha            type char30.       "Senha digitada

  if ( v_usuario is initial ) or ( v_senha is initial ).
     message i997(z1) with text-025.
     EXIT.
  endif.

* Verifica se o usuário está cadastro na Tab Senha  p/  Liberar
* registros  (Minuta de Carregamento)
  select single senha from zmmminuta03 into l_senha_banco
    where usuario eq v_usuario.

  if sy-subrc ne 0.
     message i997(z1) with text-026.
     EXIT.
  endif.

*   Encriptografa a senha
    l_senha = v_senha.
    call function 'DB_CRYPTO_PASSWORD'
         exporting
              clear_text_password          = l_senha
         importing
              encoded_password             = l_senha_encriptada
         exceptions
              crypt_output_buffer_to_small = 1
              crypt_internal_error         = 2
              crypt_truncation_error       = 3
              crypt_conversion_error       = 4
              internal_error               = 5
              others                       = 6.

    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
              with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      exit.
    endif.

  if l_senha_banco ne l_senha_encriptada(8).
     message i997(z1) with text-027.
     EXIT.
  endif.

**************************************************************
Não gera espaços do ultimo registro de cada linha ou seja se meu ultimo campo tem tamanho 20 mas só tem três caracteres ele considera o ultimo caractere como ultima 
posição.

WS_DOWNLOAD

Tente executar o seguinte comando antes da funcao:
 
PERFORM: set_trail_blanks(saplgrap) USING 'X'.

**************************************************************

Código para buscar as edições feitas no ALV:

   data: cl_alv type ref to cl_gui_alv_grid.  

  g_selfield-refresh = 'X'. 

 
  call function 'GET_GLOBALS_FROM_SLVC_FULLSCR'
 IMPORTING
   E_GRID = cl_alv.

CALL METHOD cl_alv->CHECK_CHANGED_DATA. 

**************************************************************
DATA: V_CAMPO(18), V_ITEM TYPE I.

   ADD 1 TO V_ITEM.

   concatenate 'EKPO-NETPR(' v_item ')' into v_campo.

   perform bdc_field  using: 'X' 'SAPMM06E'   '0120',
                        ' ' 'BDC_CURSOR'      v_campo,
                        ' ' 'BDC_OKCODE'      '=NP'.

   clear v_campo.
   concatenate 'EKPO-EMATN(' v_item ')' into v_campo.
   perform bdc_field  using: ' ' v_campo wa_ekpo-matnr+11(7).

**************************************************************

*       Executa a bapi_paym_item_post_item
*----------------------------------------------------------------------*

  READ TABLE t_vdarl INTO wa_vdarl INDEX 1.

  READ TABLE t_tbkk01 INTO wa_tbkk01  INDEX 1.

  t_pay_item-item_no    = '00001'.
  t_pay_item-banksitm   = 'BR'.
  t_pay_item-banklitm   = wa_tbkk01-bankl.
  t_pay_item-acextitm   = w_acnum.
  t_pay_item-refno_cr   = wa_vdarl-ranl.
  t_pay_item-refno_paym = wa_vdarl-ranl.
  t_pay_item-date_value = sy-datum.
  t_pay_item-date_post  = sy-datum.
  t_pay_item-tcur       = 'BRL'.
  t_pay_item-t_amount   = p_valor .
  t_pay_item-medium     = '0003'.
  t_pay_item-trnstype   = '0103'.
  t_pay_item-banksref   = 'BR'.
  APPEND  t_pay_item. CLEAR t_pay_item.

  t_pay_note-item_no   = '00001'.
  t_pay_note-notno     = '000'.
  t_pay_note-paym_note = wa_vdarl-ranl.
  APPEND t_pay_note.CLEAR t_pay_note.

  CLEAR: wa_vdarl,
         wa_tbkk01.

  break abap02.

*   Executa BAPI em Modo simulação
  CALL FUNCTION 'BAPI_PAYM_ITEM_POST_ITEM'
    EXPORTING
      i_x_simul    = 'X'
    IMPORTING
      e_return     = w_return
    TABLES
      t_paym_item  = t_pay_item
      t_paym_note  = t_pay_note
      e_tab_xcheck = t_tab_xcheck
      e_tab_return = t_return.

*   Checa se a conta esta bloqueada
  READ TABLE t_tab_xcheck WITH KEY xerr_aclck = 'X'.
  IF sy-subrc = 0.
    t_log-bkkrs  = w_bkkrs.
    t_log-conta  = w_acnum.
    t_log-data   = sy-datum.
    t_log-valor  = p_valor.
    t_log-return = '04'.
    t_log-descr  = 'Cta. bloqueada p/ débito'.
    APPEND t_log. CLEAR t_log.
  ELSE.
*    Executa a BAPI caso a Conta não esteja Bloqueada
    CALL FUNCTION 'BAPI_PAYM_ITEM_POST_ITEM'
      IMPORTING
        e_return     = w_return
      TABLES
        t_paym_item  = t_pay_item
        t_paym_note  = t_pay_note
        e_tab_return = t_return.

    IF w_return = '00'.
      CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'.
    ELSE.
      CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
      PERFORM f_log. "Log de erros
    ENDIF.
  ENDIF.

**************************************************************
Report's ALV através da Internet... (soh coloca os parametros,
relacionamentos de tabelas, campos... e Pronto! Gera os codigos
automaticamente...)

No site tem um exemplo...

http://www.alvrobot.com.ar/

**************************************************************
Buscando linhas selecionadas numa grid - ALV oo

DATA:
* Internal table for indexes of selected rows
gi_index_rows TYPE lvc_t_row,
* Information about 1 row
g_selected_row LIKE lvc_s_row.

Example 1: Reading index of selected row(s) and using it to read the grid table

  CALL METHOD go_grid->get_selected_rows
    IMPORTING
      et_index_rows = gi_index_rows.
  DESCRIBE TABLE gi_index_rows LINES l_lines.
  IF l_lines = 0.
    CALL FUNCTION 'POPUP_TO_DISPLAY_TEXT'
         EXPORTING
              textline1 = 'You must choose a valid line'.
    EXIT.
  ENDIF.
  LOOP AT gi_index_rows INTO g_selected_row.
     READ TABLE gi_sflight INDEX g_selected_row-index INTO g_wa_sflight.
    ENDIF.
  ENDLOOP.

******--Regra de validação***************
Transação GGB0 (ZGGBR000)

******--Regra de substituição******************************************************** início
Transação GGB1 (ZGGBS000)
Transação obbh
Transação GCX2

-----------------------
Aplicar nota 386896, após suporte package NF-e.
LFACIU04

*\  PERFORM free_interface.
*}   DELETE
*{   INSERT         ED2K901569
*Nota 386896
 DATA: I_SUBST(1).
    CALL FUNCTION 'FI_SUBSTITUTION_DOC'
       IMPORTING
            E_SUBST  = I_SUBST
       TABLES
            IO_XBSEG = XBSEG
            IO_XBKPF = XBKPF.
  IF ACCHD_FI-STATUS_NEW NE 2.
    PERFORM DOCUMENT_VALIDATION.
  ENDIF.
* Free memory. This should always happen at the end of this function.
  PERFORM FREE_INTERFACE.
-----------------------

GBTAUFI0                       / GBTAUFIB
FORM                           / SUB_ZSUBDOC
----------------------

Programa RGUGBR00 ---- organiza programa que chama regras de substituição.
Na tela de seleção, não marcar GERAR SETS.

-----
  CALL FUNCTION 'FI_SUBSTITUTION_HEADER'
    EXPORTING
      I_BKPF  = XBKPF
    IMPORTING
      E_BKPF  = XBKPF
      E_SUBST = SUBST.

  CALL FUNCTION 'FI_SUBSTITUTION_ITEM'
    EXPORTING
      I_BKPF  = XBKPF
      I_BSEG  = XBSEG
      I_EVENT = CHAR_2
    IMPORTING
      E_BSEG  = XBSEG
      E_SUBST = SUBST.

******--Regra de substituição******************************************************** fim

Vc pode chamar um perform de dentro do sapscript.
Ex. 
No sapscript
PERFORM LINE_COUNT IN PROGRAM ZMMR1002
USING &TDNAME&
USING &TDOBJECT&
USING &TDID&
USING &TDSPRAS&
CHANGING &N_LINHAS&
ENDPERFORM
 
No report
*&---------------------------------------------------------------------*
*&      Form  line_count
*&---------------------------------------------------------------------*
*       devolve ao sapscript o número de linhas em um texto
*       standard.
*----------------------------------------------------------------------*
*      <--IN_PAR     tabela com campos e valores (chaves do select)
*      -->OUT_PAR    tabela com campos e valores (n° de linhas)
*----------------------------------------------------------------------*
FORM line_count TABLES in_par STRUCTURE itcsy out_par STRUCTURE itcsy.
 
  DATA: v_name     TYPE stxh-tdname,
        v_object   TYPE stxh-tdobject,
        v_id       TYPE stxh-tdid,
        v_language TYPE stxh-tdspras,
        v_count    TYPE i.
  DATA: it_text TYPE STANDARD TABLE OF tline,
        wa_text TYPE tline.
 
  READ TABLE in_par WITH KEY 'TDNAME'.
  CHECK sy-subrc = 0.
  v_name = in_par-value.
 
  READ TABLE in_par WITH KEY 'TDOBJECT'.
  CHECK sy-subrc = 0.
  v_object = in_par-value.
 
  READ TABLE in_par WITH KEY 'TDID'.
  CHECK sy-subrc = 0.
  v_id = in_par-value.
 
  READ TABLE in_par WITH KEY 'TDSPRAS'.
  CHECK sy-subrc = 0.
  v_language = in_par-value.
 
  READ TABLE out_par WITH KEY 'N_LINHAS'.
  CHECK sy-subrc = 0.
 
  CALL FUNCTION 'READ_TEXT'
    EXPORTING
*     CLIENT                        = SY-MANDT
      id                            = v_id
      language                      = v_language
      name                          = v_name
      object                        = v_object
*     ARCHIVE_HANDLE                = 0
*     LOCAL_CAT                     = ' '
*   IMPORTING
*     HEADER                        =
    TABLES
      lines                         = it_text
*   EXCEPTIONS
*     ID                            = 1
*     LANGUAGE                      = 2
*     NAME                          = 3
*     NOT_FOUND                     = 4
*     OBJECT                        = 5
*     REFERENCE_CHECK               = 6
*     WRONG_ACCESS_TO_ARCHIVE       = 7
*     OTHERS                        = 8
            .
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
 
  LOOP AT it_text INTO wa_text.
    IF wa_text-tdformat = '*'.
      ADD 1 TO v_count.
    ENDIF.
  ENDLOOP.
 
*  SELECT SINGLE tdtxtlines FROM stxh INTO out_par-value
*    WHERE tdobject = v_object
*      AND tdname   = v_name
*      AND tdid     = v_id
*      AND tdspras  = v_language.
*  CHECK sy-subrc = 0.
 
  MOVE v_count TO out_par-value(2).
  MODIFY out_par INDEX 1.
 

ENDFORM.                    " line_count

**************************************************************
Search help dinânmico.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS:
s_bkkrs	FOR bkk50-bkkrs OBLIGATORY,
s_conta	FOR bkk42-acnum_ext,
s_prod	       FOR fipr_product-prodext MATCHCODE OBJECT zbca010,
s_lim		FOR bkke1-limsum NO-EXTENSION,
s_partn       FOR bkk45-partner.
PARAMETERS:
p_back   AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK b1.

at selection-screen on value-request for s_partn-low.
  perform f4_partner using s_partn-low.

at selection-screen on value-request for s_partn-high.
  perform f4_partner using s_partn-high.



form f4_partner using p_partn.

  data: begin of wa_valuetab,
          fieldtext like dfies-fieldtext,
        end of wa_valuetab.

types: begin of ty_partn,
        partner     like but100-partner,
        rltyp       like but100-rltyp,
        dfval       like but100-dfval,
      end   of ty_partn.

  data: t_valuetab     like standard table of wa_valuetab,
        wa_fields      like help_value,
        t_fields       type standard table of help_value,
        t_partn        type table of ty_partn,
        wa_partn       type ty_partn.

    refresh: t_fields, t_valuetab, t_partn.

*   Monta as colunas de exibição
    wa_fields-tabname    = 'BUT100'.
    wa_fields-fieldname  = 'PARTNER'.
    wa_fields-selectflag = 'X'.
    append wa_fields to t_fields.
    clear wa_fields.

    wa_fields-tabname   = 'BUT100'.
    wa_fields-fieldname = 'RLTYP'.
    append wa_fields to t_fields.
    clear wa_fields.

    wa_fields-tabname   = 'BUT100'.
    wa_fields-fieldname = 'DFVAL'.
    append wa_fields to t_fields.
    clear wa_fields.

*   Lê tabela com os dados a serem exibidos p/ o match code

*   Selecionando somente gerente das contas
  select partner rltyp dfval
     into table t_partn
       from but100
     where rltyp = 'BKK200'.

    if not t_partn[] is initial.

      loop at t_partn into wa_partn.

        wa_valuetab-fieldtext = wa_partn-partner.
        append wa_valuetab to t_valuetab.
        clear wa_valuetab.

        wa_valuetab-fieldtext = wa_partn-rltyp.
        append wa_valuetab to t_valuetab.
        clear wa_valuetab.

        wa_valuetab-fieldtext = wa_partn-dfval.
        append wa_valuetab to t_valuetab.
        clear wa_valuetab.

      endloop.

    endif.

* Chama a função para a ajuda de pesquisa montada
  call function 'HELP_VALUES_GET_WITH_TABLE'
    exporting
      display                       = space
      fieldname                     = 'partner'
      tabname                       = 'BUT100 '
      titel                         = 'Gerentes de contas'
      show_all_values_at_first_time = 'X'
    importing
      select_value                  = p_partn   "Gerente
    tables
      fields                        = t_fields
      valuetab                      = t_valuetab.

endform.

******************************************************************************************

Refresh ALV não OO

  DATA: CL_ALV TYPE REF TO CL_GUI_ALV_GRID.

  CLASS CL_GUI_CFW DEFINITION LOAD.

* CHAMANDO MÉTODO DE ATUALIZAÇÃO/EVENTOS DO ALV
  CALL METHOD CL_GUI_CFW=>DISPATCH.


  CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
       IMPORTING
            E_GRID = CL_ALV.

  CALL METHOD CL_ALV->CHECK_CHANGED_DATA.
---------------------

No user_command:

  selfield-refresh = 'X'.  "Atualiza a grid
  "Depois do refresh, o foco fica na mesma coluna que estava antes

  "Depois do refresh, o foco fica na mesma coluna que estava antes
  selfield-col_stable = 'X'.

  "Depois do refresh, o foco fica (permanece, volta) na mesma linha que estava antes
  selfield-row_stable = 'X'.


Para o ALV OO:

CALL METHOD <ref.var. to CL_GUI_ALV_GRID>->refresh_table_display  
   EXPORTING   
      IS_STABLE      = <structure of type LVC_S_STBL>   
      I_SOFT_REFRESH = <variable of type CHAR01>.

IS_STABLE => Estabiliza o scrool down para linhas e colunas ao realizar o refresh.   I_SOFT_REFRESH=> Usado apenas em aluns casos especiais onde não deverá mudar nenhuma totalização, filtro ou ordenação dos registros, usado quando não foram alterados os dados e vc precisa apenas atualizar o layout, por exemplo.

Para o ALV normal:
******************************************************************************************
Gera planilha excel: 
  CALL FUNCTION 'SAP_CONVERT_TO_XLS_FORMAT'
      EXPORTING
        i_field_seperator          = 'X'
*        i_line_header              = t_header
        i_filename                 = p_wind
*       I_APPL_KEEP                = ' '
      TABLES
        i_tab_sap_data             = reg_aux.


Le planilha excel.

 CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'.

Ou:

  DATA: t_excel  TYPE TABLE OF kcde_cells WITH HEADER LINE.

  CALL FUNCTION 'KCD_CSV_FILE_TO_INTERN_CONVERT'
    EXPORTING
      i_filename      = p_file
      i_separator     = ';'
    TABLES
      e_intern        = t_excel
    EXCEPTIONS
      upload_csv      = 1
      upload_filetype = 2
      OTHERS          = 3.

  IF sy-subrc NE 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

* Indica primeira linha como linha corrente
  READ TABLE t_excel INDEX 1.
  v_currentrow = t_excel-row.

  LOOP AT t_excel.
*   Limpa valores para próxima linha
    IF t_excel-row NE v_currentrow.
      APPEND wa_metas TO t_metas.
      CLEAR wa_metas.
      v_currentrow = t_excel-row.
    ENDIF.

    CASE t_excel-col.
      WHEN '0001'."Vendedor
        wa_metas-kunnr   = t_excel-value.
*       Converte código do Vendedor
        PERFORM z_conversion_alpha_input CHANGING wa_metas-kunnr.
      WHEN '0002'."Canal
        wa_metas-pvksm   = t_excel-value.
      WHEN '0003'."Hierarquia
        wa_metas-prodh   = t_excel-value.
      WHEN '0004'."Meta Quantidade
        wa_metas-metaqtd = t_excel-value.
      WHEN '0005'."Meta Valor
        wa_metas-metavlr = t_excel-value.
      WHEN '0006'."Objetivo Quantidade
        wa_metas-objtqtd = t_excel-value.
      WHEN '0007'."Objetivo Valor
        wa_metas-objtvlr = t_excel-value.
    ENDCASE.

    APPEND wa_metas TO t_metas.

  ENDLOOP.

******************************************************************************************
converte texto unidade de medida.

          call function 'CONVERSION_EXIT_CUNIT_OUTPUT'
            exporting
              input      = ti_vbrp-meins
              language   = sy-langu
            importing
              short_text = ti_vbrp-meins.
******************************************************************************************
* Dispara o evento YEVUSD_NF_EXP_GECEX ao qual está amarrado o JOB
* INTERFACE_SD_NF_EXP_GECEX que acionará a execução do programa
* YOSSD_NF_EXPORTACAO_GECEX.

    call function 'BP_EVENT_RAISE'
      exporting
        eventid                = 'YEVUSD_NF_EXP_GECEX'
        eventparm              = ' '
        target_instance        = ' '
      exceptions
        bad_eventid            = 1
        eventid_does_not_exist = 2
        eventid_missing        = 3
        raise_failed           = 4
        others                 = 5.
******************************************************************************************
TYPES: BEGIN OF type_zsdt043.
        INCLUDE STRUCTURE zsdt043.
TYPES:   name1 TYPE kna1-name1,
         stcd1 TYPE kna1-stcd1,
       END OF type_zsdt043.
******************************************************************************************
Zipar no UNIX

DATA: BEGIN OF t_retorno OCCURS 10,
        ret(255) TYPE C,
      END OF t_retorno.

CALL 'SYSTEM' ID 'COMMAND' FIELD 'gzip /tmp/arquivo.txt'
              ID 'TAB'     FIELD t_retorno-*sys*.

******************************************************************************************
Abre caixa diálogo, pra buscar arquivos (orientado a objetos)

parameters p_path like rlgrap-filename.

at selection-screen on value-request for p_path.
  data: lc_arq like ibipparms-path.
  call function 'F4_FILENAME'
    importing
      file_name = lc_arq.

  p_path = lc_arq.

-----
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_any.

  PERFORM zf_get_filename CHANGING p_any.

FORM zf_get_filename  CHANGING p_file.

  data: l_mask type string.

  DATA: t_filename TYPE filetable,
        w_filename LIKE LINE OF t_filename,
        l_title    TYPE string,
        i_rc       TYPE i.

  l_title = text-001.

  if p_anap = 'X'.
    l_mask = 'Arq texto|*.TXT'.
  else.
    l_mask = 'Arq excel|*.XLSX|*.XLS'.
  endif.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = l_title
      default_extension       = l_mask
      file_filter             = l_mask
      initial_directory       = p_file
      multiselection          = ' '
    CHANGING
      file_table              = t_filename
      rc                      = i_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  CHECK sy-subrc IS INITIAL.

  READ TABLE t_filename INTO w_filename INDEX 1.

  IF sy-subrc IS INITIAL.
    p_file = w_filename-filename.
  ENDIF.

ENDFORM.

***************************************************************************************************************
directory
Função DSVAS_DOC_FILENAME_SPLIT: Informando uma pasta ou diretório e arquivo, retorna nome de: pasta, arquivo, extensão
***************************************************************************************************************

Alterar código fonte, em ambiente fechado

TRINT_CORR_CHECK   ------ colocar break no subrc
TRINT_CORR_INSERT  ------ colocar break no subrc

Qdo entrar no programa, pela se38, alterar os valores dos subrc's
******************************************************************************************
FIELD-SYMBOLS: <fs_vg>     TYPE ANY,
               <fs_uf>     TYPE ANY,
               <fs_selopt> TYPE table,
               <fs_wasel>  TYPE ANY,
               <fs_var>    TYPE ANY,
               <fs_rem>    TYPE ANY,
               <fs_traco>  TYPE ANY.

  DATA: vl_where         TYPE string,
        vl_str           TYPE string.

  CONCATENATE 'S_'  p_sel '[]'     INTO vl_str.
  ASSIGN (vl_str) TO <fs_selopt>.

  CONCATENATE 'S_'  p_sel          INTO vl_str.
  ASSIGN (vl_str) TO <fs_wasel>.

  CONCATENATE 'VG_' p_sel          INTO vl_str.
  ASSIGN (vl_str) TO <fs_vg>.

  CHECK: ( <fs_vg>     IS ASSIGNED ),
         ( <fs_selopt> IS ASSIGNED ) AND ( NOT <fs_selopt> IS INITIAL ).

  LOOP AT <fs_selopt> INTO <fs_wasel>.

    DO 2 TIMES.
      IF ( sy-index EQ 1 ).
        CONCATENATE 'S_' p_sel '-LOW'  INTO vl_str.
        ASSIGN (vl_str) TO <fs_var>.
      ELSE.
        CONCATENATE 'S_' p_sel '-HIGH' INTO vl_str.
        ASSIGN (vl_str) TO <fs_var>.
      ENDIF.

      CHECK ( <fs_var> IS ASSIGNED ) AND ( NOT <fs_var> IS INITIAL ).

      CONCATENATE '(' p_field 'EQ' vl_str  ')'
                                   INTO vl_where SEPARATED BY space.
      SELECT SINGLE (p_field)
        FROM (p_table)
        INTO <fs_vg>
       WHERE (vl_where).

      CHECK ( sy-subrc NE 0 ).
      MESSAGE e899(mm) WITH p_text <fs_var> text-e00.

    ENDDO.

  ENDLOOP.


********************************************************************
   V_LENGTH = strlen( p_frede ).
********************************************************************
  data vl_field  type c length 10.
  field-symbols: <fs_uf>     type any.

      loop at it_estado into wa_estado
                     where ( tknum eq wa_aux-tknum ).

        concatenate 'vg_uf' vg_cont into vl_field.
        assign (vl_field) to <fs_uf>.
        if ( <fs_uf> is assigned ).
          <fs_uf> = wa_estado-uf.
        endif.

        vg_cont = vg_cont + 1.

      endloop.
********************************************************************
Criação de SETs

Transação GS01/GS02/GS03 - pode ser usada p/ substituir criação de tabela Z.

Tabela SETLEAF - campo SETNAME = Z?????????


data: r_rufnm type range of pa0002-rufnm.

     select valsign valoption valfrom valto
            into table r_rufnm
        from setleaf
             where setname eq 'ZFI_SUPERAPROV'.


********************************************************************
Textos de domínios.

select single ddtext into v_texto
       from dd07t
            where domname = 'ZD_STATUS' and    "Nome do domínio
               domvalue_l = '01'.              "Valor do domínio

Ou:

  data: begin of it_acao occurs 0,
          domvalue_l  type dd07t-domvalue_l,
          ddtext       type dd07t-ddtext,
        end of it_acao.


    "Busca os textos dos valores do domínio ZACAO
    select domvalue_l ddtext into table it_acao
           from dd07t
                where domname = 'ZACAO'.   "Nome do domínio

    sort it_acao by domvalue_l.

    "busca texto do domínio do campo ACAO
    read table it_acao with key domvalue_l = it_ztrt003-acao binary search.

    it_pos-descricao = it_acao-ddtext.

Ou:

data: it_bfart           like standard table of dd07v.

  call function 'DDUT_DOMVALUES_GET'
    exporting
      name      = 'BFART'
    tables
      dd07v_tab = it_bfart.

  sort it_bfart by domvalue_l.

  read table it_bfart into wa_bfart
                      with key domvalue_l = vg_bfart binary search.

  write it_bfart-ddtext.

******************************************************
pra não dar erro, divindo por zero

   try.
        vg_brgew = ( wa_vbap-brgew / wa_vbap-kwmeng ).
     catch cx_sy_zerodivide.
        vg_brgew = wa_vbap-brgew.
    endtry.

Ou...
data: v_ex_ref                type ref to cx_root,
      v_msg_text              type string.

      try .
          <fs_konv>-kwert = ( <fs_konv>-kwert * v_menge ) / v_kcmeng .
        catch cx_sy_zerodivide into v_ex_ref.
          v_msg_text = v_ex_ref->get_text( ).
        catch cx_sy_conversion_no_number into v_ex_ref.
          v_msg_text = v_ex_ref->get_text( ).
        catch cx_root into v_ex_ref.
          v_msg_text = v_ex_ref->get_text( ).
      endtry.


******************************************************
Retorna o valor em moeda estrangeira

DATA: XRATE  TYPE F,
      FAMT   TYPE P,
      FFACT  TYPE F,
      LFACT  TYPE F.

PARAMETERS: FCURR  LIKE TCURC-WAERS,
            LCURR  LIKE TCURC-WAERS,
            LAMT   TYPE P.

CALL FUNCTION 'CONVERT_TO_FOREIGN_CURRENCY'
     EXPORTING
          DATE             = SY-DATUM
          FOREIGN_CURRENCY = FCURR
          LOCAL_AMOUNT     = LAMT
          LOCAL_CURRENCY   = LCURR
     IMPORTING
          EXCHANGE_RATE    = XRATE
          FOREIGN_AMOUNT   = FAMT
          FOREIGN_FACTOR   = FFACT
          LOCAL_FACTOR     = LFACT
     EXCEPTIONS
          NO_RATE_FOUND    = 1
          OVERFLOW         = 2
          NO_FACTORS_FOUND = 3
          NO_SPREAD_FOUND  = 4
          DERIVED_2_TIMES  = 5
          OTHERS           = 6.

IF SY-SUBRC EQ 0.
  WRITE:/ LAMT, 'CONVERTED FROM', LCURR, 'TO', FCURR, 'IS:', FAMT.
  WRITE:/ 'EXCHANGE RATE:', XRATE,
        / 'FOREIGN FACTOR:', FFACT,
        / 'LOCAL FACTOR:', LFACT.
ELSE.
  WRITE:/ LAMT, 'NOT CONVERTED'.
ENDIF.

******************************************************
Atualiza valor do documento (converte o valor em ufir, por exemplo):

  MOVE-CORRESPONDING bkpf TO icurr.
  MOVE-CORRESPONDING bseg TO icurr.
  icurr-kurst = t003-kurst.
  CALL FUNCTION 'FI_CURRENCIES_CONVERT'
    EXPORTING
      i_icurr = icurr
    IMPORTING
      e_icurr = icurr.
  MOVE-CORRESPONDING icurr TO bseg.

******************************************************
Macete pra atualizar campos disabled de tabelas Z, em tela diálogo atualizada

Os campos de dt e usuário de inclusão, dt e usuário alteração, são modificados nesta rotina.

Exemplo p/ tabela ZSDT017, grupo de função ZSDT002

    MODULE SET_UPDATE_FLAG ON CHAIN-REQUEST. "Este é gerado automático
*   crie o módulo abaixo
    module z_test_fields on chain-request.


Crie na include LZSDT002I01
MODULE z_test_fields INPUT.

  DATA: t_zsdt017 TYPE zsdt017 OCCURS 0 WITH HEADER LINE.

  SELECT *
    FROM zsdt017
    INTO TABLE t_zsdt017
    WHERE vkorg = zsdt017-vkorg  AND
          auart = zsdt017-auart .

  IF sy-subrc = 0.
    zsdt017-zdtaalte = sy-datum.
    zsdt017-zusualte = sy-uname.
  ELSE.
    zsdt017-zdtaincl = sy-datum.
    zsdt017-zusuincl = sy-uname.
    zsdt017-zdtaalte = space.
    zsdt017-zusualte = space.
  ENDIF.

ENDMODULE.                 " z_test_fields  INPUT


Pode ser criado um module pra validação de campos, como abaixo:
Exemplo tabela ZSDT019

MODULE z_test_fields3 INPUT.

  DATA: t_zsdt019 TYPE zsdt019 OCCURS 0 WITH HEADER LINE.

  IF zsdt019-zdtaini > zsdt019-zdtafim.
    PERFORM set_pf_status USING 'ERROR'.
    MESSAGE e000(sv) WITH text-002.
    CLEAR: zsdt019-zdtaini  ,
           zsdt019-zdtafim  .
  ENDIF.

  SELECT *
    FROM zsdt019
    INTO TABLE t_zsdt019
    WHERE vkorg    = zsdt019-vkorg    AND
          vkbur    = zsdt019-vkbur    AND
          vkgrp    = zsdt019-vkgrp    AND
          zusuario = zsdt019-zusuario AND
          zbloq    = zsdt019-zbloq .

  IF sy-subrc = 0.
    zsdt019-zdtaalte = sy-datum.
    zsdt019-zusualte = sy-uname.
  ELSE.
    zsdt019-zdtaincl = sy-datum.
    zsdt019-zusuincl = sy-uname.
  ENDIF.

  IF zsdt019-zbloq = 'DT'.
    IF zsdt019-zkschl NE ''.
      PERFORM set_pf_status USING 'ERROR'.
      MESSAGE e000(sv) WITH text-001.
      CLEAR zsdt019-zkschl .
    ENDIF.
  ENDIF.

ENDMODULE.                 " z_test_fields3  INPUT

Pra inibir todos os botões (NOVA ENTRADA, exibir/modificar...)
PROCESS BEFORE OUTPUT.
 MODULE LISTE_INITIALISIEREN.
 LOOP AT EXTRACT WITH CONTROL
  TCTRL_ZSDT018 CURSOR NEXTLINE.
   MODULE LISTE_SHOW_LISTE.
 ENDLOOP.
  module z_exclude_icons.    "usar este módulo

Na include LZSDT002I01
MODULE z_exclude_icons OUTPUT.

  IF sy-DYNNR = '0002'.

    PERFORM set_pf_status USING 'REPLACE'.

  ENDIF.

ENDMODULE.                 " z_exclude_icons  OUTPUT

--------------
Tornar obrigatório (validar campos em branco) antes de gravar (incluir ou modificar) registro em tela de diálogo:

    MODULE SET_UPDATE_FLAG ON CHAIN-REQUEST.   "este é standard
    module f_checa_obs.
   ENDCHAIN.


module f_checa_obs input.

  if ( status-action = 'A'          "Novo registro
                      or status-action = 'U' )  "Modificando registro...
       and anzeigen = 'S'
          and status-delete is initial
             and <status>-upd_flag = 'X'
                and zsdt001-obs is initial.
    message e000(zsd) with 'Favor entrar com observações do registro'.
  endif.

endmodule.

******************************************************
SM30 - Alterações no programa gerado, pela SE11.
* Chama outra tela, modifica a tabela Z, e qdo volta, mostra os dados * alterados na primeira tela (no table control a standard gerada pela * SE11)

--- início
process before output.

  module liste_initialisieren.

* Verifica se houve alteração em massa (na tela '0002').
* Se sim, limpa estruturas standard e lê novamente a tab ZSDT079
  module z_lista_tudo.

  loop at extract with control
   tctrl_zsdt079 cursor nextline.
    module liste_show_liste.
  endloop.

process after input.
  module liste_exit_command at exit-command.

* Se clicar botão 'Dt faturamento', chama tela '0002'
*  module z_data_faturamento.

  module liste_before_loop.
  loop at extract.
    module liste_init_workarea.
    chain.
      field zsdt079-werks .
      field zsdt079-vstel .
      field zsdt079-fkdat .
      module set_update_flag on chain-request.
      module z_valida_data on chain-request.
      module z_prepara_histor_modif on chain-request.
    endchain.
    field vim_marked module liste_mark_checkbox.
    chain.
      field zsdt079-werks .
      field zsdt079-vstel .
      module liste_update_liste.
    endchain.
    module z_prepara_histor_delete.
    module z_selecionados.
  endloop.

* Se clicar botão 'Dt faturamento', chama tela '0002'
  module z_data_faturamento.

  module liste_after_loop.

-- modules Z
module z_lista_tudo output.

* Verifica se houve alteração em massa (na tela '0002').
* Se sim, limpa estruturas standard e lê novamente a tab ZSDT079
  check v_flag_data = 'X'.

  select * from zsdt079 into table t_zsdt079.

  check sy-subrc = 0.

* Limpa tabelas internas, standard.
  refresh: total, extract.

  LOOP AT t_zsdt079.
    extract = total = t_zsdt079.
    APPEND: extract,
            total.
  ENDLOOP.

  clear v_flag_data.

endmodule.                 " z_lista_tudo  OUTPUT

module z_valida_data input.

  data: l_fkdat like vbrk-fkdat,
        l_vbeln like vbrk-fkdat,
        l_werks like vbrp-werks,
        l_vstel like vbrp-vstel.

  data: begin of t_vbrk occurs 0,
        fkdat like vbrk-fkdat,
        vbeln like vbrk-vbeln,
        end of t_vbrk.

*	Se existe algum registro na tabela VBRK referente a NF, e este
* não se encontra estornado (FKSTO = Vazio) e data
*  de faturamento (FKDAT) for maior que a dt informada pelo usuário
*  e centro (VBRP-WERKS) igual ao centro da tabela Z e Local de
*  expedição (VBRP-VSTEL) igual ao local de expedição da tabela Z,
*  não permitir gravação.

  check not zsdt079-fkdat is initial.
  clear: l_fkdat, l_vbeln, l_werks, l_vstel.

  select max( k~fkdat ) k~vbeln
           into table t_vbrk
              from vbrk as k
                 inner join vbrp as p
                   on k~vbeln = p~vbeln
                   where k~fkdat > zsdt079-fkdat and
                         k~fksto = space         and
                         p~werks = zsdt079-werks and
                         p~vstel = zsdt079-vstel
         group by k~fkdat k~vbeln.

  if sy-subrc = 0.
    perform set_pf_status using 'ERROR'.
    message e000(zsd) with text-001.
  endif.

endmodule.                 " z_valida_data  INPUT

*&---------------------------------------------------------------------*
*&      Module  z_prepara_histor_modif  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module z_prepara_histor_modif input.

  check function ne 'DELE'.

  clear: t_zsdt080, wa_zsdt079.

  select single *
    from zsdt079
         into wa_zsdt079
    where werks = zsdt079-werks  and
          vstel = zsdt079-vstel.

  if sy-subrc = 0.
    t_zsdt080-werks_new = zsdt079-werks.
    t_zsdt080-vstel_new = zsdt079-vstel.
    t_zsdt080-fkdat_new = zsdt079-fkdat.
    t_zsdt080-werks_old = wa_zsdt079-werks.
    t_zsdt080-vstel_old = wa_zsdt079-vstel.
    t_zsdt080-fkdat_old = wa_zsdt079-fkdat.
    t_zsdt080-change_ind = 'U'.
  else.
    t_zsdt080-werks_new = zsdt079-werks.
    t_zsdt080-vstel_new = zsdt079-vstel.
    t_zsdt080-fkdat_new = zsdt079-fkdat.
    t_zsdt080-change_ind = 'I'.
  endif.

  t_zsdt080-mandt = sy-mandt.
  append t_zsdt080.

endmodule.                 " z_prepara_histor_modif  INPUT

*&---------------------------------------------------------------------*
*&      Module  z_prepara_histor_delete  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module z_prepara_histor_delete input.

  check ( function = 'DELE' )
       and ( VIM_MARKED = 'X' ).

  clear: t_zsdt080, wa_zsdt079.

  select single *
    from zsdt079
         into wa_zsdt079
    where werks = zsdt079-werks  and
          vstel = zsdt079-vstel.

  if sy-subrc = 0.
    t_zsdt080-werks_new = zsdt079-werks.
    t_zsdt080-vstel_new = zsdt079-vstel.
    t_zsdt080-fkdat_new = zsdt079-fkdat.
    t_zsdt080-werks_old = wa_zsdt079-werks.
    t_zsdt080-vstel_old = wa_zsdt079-vstel.
    t_zsdt080-fkdat_old = wa_zsdt079-fkdat.
    t_zsdt080-change_ind = 'D'.
  endif.

  t_zsdt080-mandt = sy-mandt.
  append t_zsdt080.

endmodule.                 " z_prepara_histor_delete  INPUT

module z_selecionados input.

  check VIM_MARKED = 'X'.

  clear t_selecionados.

    t_selecionados-werks = zsdt079-werks.
    t_selecionados-vstel = zsdt079-vstel.

  append t_selecionados.

endmodule.

*&---------------------------------------------------------------------*
*&      Module  z_data_faturamento  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module z_data_faturamento input.

  if sy-ucomm = 'DATF'.
     if t_selecionados[] is initial.
        message i000(zsd) with 'Nenhuma linha selecionada!'.
        exit.
     else.
        call screen '0002'
          STARTING AT 15 05 ENDING AT 70 14.
     endif.
  endif.

endmodule.                 " z_data_faturamento  INPUT

*&---------------------------------------------------------------------*
*&      Module  user_command_0002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
module user_command_0002 input.

  if sy-ucomm = 'BACK'
     or sy-ucomm = 'EXIT'
        or sy-ucomm = 'CANC'
            or sy-ucomm = 'VOLTA'.
     clear: okcode, t_selecionados[].
     refresh t_selecionados[].
     SET SCREEN 0.
     LEAVE SCREEN.
  endif.

  if sy-ucomm = 'EXEC'.

     if v_fkdat is initial.
        message i000(zsd) with 'Data inválida!'.
        exit.
     endif.

     loop at t_selecionados.

       update zsdt079 set fkdat = v_fkdat
              where werks = t_selecionados-werks and
                    vstel = t_selecionados-vstel.

     endloop.

     commit work.

     v_flag_data        = 'X'.
     STATUS-MODE        = 'L'.
     STATUS-ACTION      = 'U'.
     vim_tabctrl_active = STATUS-DATA = 'X'.
     nextline = X_HEADER-LISTE = firstline = 1.
     destpage = mark_extract = mark_total = MAXLINES = 0.
     function = 'BACK'.
     L        = 2.
*     MAXLINES = 9
*STATUS	                                   	EULG
     CLEAR: vim_special_mode, vim_single_entry_function, mark_extract,
            vim_called_by_cluster, vim_import_profile, replace_mode,
            X_HEADER-SELECTION, X_HEADER-DELMDTFLAG, <action>.
*     PERFORM liste_back.
     clear: okcode, t_selecionados[].
     refresh t_selecionados[].
     SET SCREEN 0.
     LEAVE SCREEN.

  endif.

endmodule.                 " user_command_0002  INPUT

--- Fim

**************************************************************************
Melhor maneira de acesso com uma Nota Fiscal X Faturamento, utilize a Visão M_J1BAB.

**************************************************************

Mudar valor num campo, numa determinada tela.

SET_DYNP_VALUE


Buscando os valores de uma tela de seleção.

Data: t_dynpfields   type standard table of dynpread with header line.

form busca_campo .
  refresh: t_dynpfields.
  clear: w_bkkrs, w_conta, w_perio.

  t_dynpfields-fieldname    = 'P_BKKRS'.
  append t_dynpfields.
  t_dynpfields-fieldname    = 'P_CONTA'.
  append t_dynpfields.
  t_dynpfields-fieldname    = 'P_BENEFI'.
  append t_dynpfields.
  t_dynpfields-fieldname    = 'P_APELI'.
  append t_dynpfields.


  call function 'DYNP_VALUES_READ'
    exporting
      dyname               = sy-repid
      dynumb               = sy-dynnr
    tables
      dynpfields           = t_dynpfields
    exceptions
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      undefind_error       = 7
      others               = 8.

  sort t_dynpfields by fieldname ascending.

  read table t_dynpfields with key fieldname = 'P_BKKRS'.
  w_bkkrs  = t_dynpfields-fieldvalue.
  read table t_dynpfields with key fieldname = 'P_CONTA'.
  w_conta  = t_dynpfields-fieldvalue.
  read table t_dynpfields with key fieldname = 'P_BENEFI'.
  w_benefi  = t_dynpfields-fieldvalue.
  read table t_dynpfields with key fieldname = 'P_APELI'.
  w_apeli   = t_dynpfields-fieldvalue.


  translate: w_bkkrs  to upper case.


endform.                    " busca_campo

**************************************************************
desabilitar um botão definido no 
Status Gui de um programa standard.

FORM define_pfstatus.

TYPES: BEGIN OF ltype_tab,
fcode LIKE rsmpe-func,
END OF ltype_tab.

DATA: lt_rest TYPE STANDARD TABLE OF ltype_tab
WITH NON-UNIQUE DEFAULT KEY
INITIAL SIZE 10,
le_rest TYPE ltype_tab,
* flags para os botoes - estes são exemplos
ln_edita TYPE n,
ln_novo TYPE n,
ln_save TYPE n.

* Default
CLEAR: ln_edita,
ln_novo,
ln_save.

* edita
IF we_fl_edita = 'X'.
lc_edita = 1.
lc_novo = 1.
lc_save = 1.

* apenas consulta
ELSEIF we_fl_cons = 'X'.
lc_edita = 0.
lc_novo = 1.
lc_save = 0.
ENDIF.

* Aplica restrições
IF lc_edita = 0.
le_rest-fcode = 'EDITA'.
APPEND le_rest TO lt_rest.
ENDIF.

IF lc_novo = 0.
le_rest-fcode = 'NOVO'.
APPEND le_rest TO lt_rest.
ENDIF.

IF lc_save = 0.
le_rest-fcode = 'SAVE'.
APPEND le_rest TO lt_rest.
ENDIF.

SET PF-STATUS 'GERAL' EXCLUDING lt_rest.

ENDFORM.

*****************************************************************

Colocar campo high do select-options, obrigatório.

at selection-screen output.
  loop at screen.
    if screen-name = 'S_DATA-HIGH'.
      screen-required = 1.
      modify screen.
    endif.
  endloop.

*****************************************************************
    WRITE WA_BSID-BLDAT TO WA_BMCARTEI-DT_EMISSAO
          USING EDIT MASK '__/__/____'.
*****************************************************************

a tabela MBEWH guarda o histórico de saldos do último dia de cada mês.

função standard "J_1B_READ_MBEW_MBEWH" informa o saldo inicial e o final.
 
* read MBEW information to determine initial/final quantities
SELECT SINGLE * FROM mbew
WHERE matnr = p_i_mseg_matnr
AND bwkey = p_i_mseg_werks
AND bwtar = p_i_mseg_bwtar.
IF only_mbew IS INITIAL.
* read mbewh information (if available)
CALL FUNCTION 'J_1B_READ_MBEW_MBEWH'
EXPORTING
i_mbew = mbew
IMPORTING
e_mbew = mbew
EXCEPTIONS
no_selection = 1
OTHERS = 2.
ENDIF.

O primeiro select é pra buscar os dados na mbew com a chave CÓDIGO MATERIAL/CENTRO OU PLANTA/VALUATION TYPE, depois de carregar estes dados na MBEW chamo a função standard passando os dados do select, e o retorno da função me passa o saldo inicial MBEW-VMSAL e saldo final MBEW-LKBUM.

*****************************************************************

  v_semaforo = v_semaforo + 1.

* Executar a BAPI
  call function 'ZTR_MIRO'
    starting new task 'ZTRR008'    "sy-repid
    performing return_info on end of task
    exporting
      ncm        = v_ncm
      headerdata = it_header
*    IMPORTING
*      invoicedocnumber = v_invoicedocnumber
*      fiscalyear       = v_fiscalyear
    tables
      itemdata         = it_itens
**      glaccountdata    = it_contas
      withtaxdata      = it_taxdata
      return           = it_return_aux.

*  PERFORM export_ncm_clear.
*     Controla New Task
  wait until v_semaforo = 0.

form return_info using taskname.

  receive results from function 'ZTRR008'
   importing
     invoicedocnumber = v_invoicedocnumber
     fiscalyear       = v_fiscalyear
   tables
     itemdata         = it_itens
*      glaccountdata    = it_contas
     withtaxdata      = it_taxdata
     return           = it_return_aux.

  v_semaforo = v_semaforo - 1.

endform.                    " return_info

*****************************************************************
        call function 'ZMMF001' starting new task 'VLBAPI'
          exporting
            p_type  = 'M'
            p_mblnr = wa_xmseg-smbln
            p_mjahr = wa_xmseg-sjahr.

*****************************************************************
Busca nome de empresa/centro

  SELECT werks name1 bukrs
         INTO CORRESPONDING FIELDS OF TABLE organ
         FROM t001w INNER JOIN t001k
         ON t001w~bwkey = t001k~bwkey
         WHERE werks IN werks
           AND bukrs IN bukrs.
  SORT organ BY werks.

*****************************************************************

  data: l_data     like line of it_data,
        timestampl type timestampl,
        tiemstamp  type cacstimestamp,
        l_text(19) type c.

* buscar timestapl
  clear l_text.
  get time stamp field timestampl.
  write : timestampl to l_text time zone 'BRAZIL'.

* Time Stamp (YYYY.MM.DD hh:mm:ss)
  call function 'CONVERSION_EXIT_TSTPS_INPUT'
    exporting
      input  = l_text
    importing
      output = tiemstamp.

-----
Função ABI_TIMESTAMP_CONVERT_INTO ---> Convert date and time into timestamp
ABI_TIMESTAMP_CONVERT_FROM ---> convert timestamp para data e hora
*****************************************************************

      DATA: l_jobcount1 LIKE tbtcjob-jobcount, " Job's number
            l_jobname1  LIKE tbtcjob-jobname,  " Job's name
            rspar_tab1  TYPE TABLE OF rsparams,
            rspar_line1 LIKE LINE OF rspar_tab,
            l_hora1     LIKE sy-uzeit.

      CONCATENATE 'CRIAÇÃO/ALTERAÇÃO OV - ' sy-datum+6(2) '/' sy-datum+4(2) '/'
                  sy-datum(4) ' - ' sy-uzeit(2) ':' sy-uzeit+2(2) ':'
                  sy-uzeit+4(2) ':' INTO l_jobname1.

      CALL FUNCTION 'JOB_OPEN'
        EXPORTING
          jobname          = l_jobname1
        IMPORTING
          jobcount         = l_jobcount1
        EXCEPTIONS
          cant_create_job  = 1
          invalid_job_data = 2
          jobname_missing  = 3
          OTHERS           = 4.

      DATA : vl_seqsvo LIKE zcot003-seqsvo.

      IMPORT vl_seqsvo FROM MEMORY ID 'ZZS'.

      rspar_line1-selname = 'P_CODSEV'.
      rspar_line1-kind    = 'S'.
      rspar_line1-sign    = 'I'.
      rspar_line1-option  = 'EQ'.
      rspar_line1-low     = l_codsev.
      APPEND rspar_line1 TO rspar_tab1.

      rspar_line1-selname = 'P_SEQSVO'.
      rspar_line1-low     = vl_seqsvo.
      APPEND rspar_line1 TO rspar_tab1.

*    l_hora = sy-uzeit + 5.
*    l_hora1 = sy-uzeit + 2.

      SUBMIT zcor003  WITH SELECTION-TABLE rspar_tab1
                      AND RETURN
                      USER sy-uname
                      VIA JOB l_jobname1
                      NUMBER  l_jobcount1.

      CALL FUNCTION 'JOB_CLOSE'
        EXPORTING
          jobcount             = l_jobcount1
          jobname              = l_jobname1
          strtimmed            = 'X'
*        sdlstrtdt            = sy-datum
*        sdlstrttm            = l_hora1
        EXCEPTIONS
          cant_start_immediate = 1
          invalid_startdate    = 2
          jobname_missing      = 3
          job_close_failed     = 4
          job_nosteps          = 5
          job_notex            = 6
          lock_failed          = 7
          OTHERS               = 8.
*****************************************************************
data: begin of ty_fkart,
         fkart               type vbrk-fkart,
      end of ty_fkart,

data  t_fkart         type table of ty_fkart     with header line.

split wa_zsdt001-campo at ';' into table t_fkart.

*****************************************************************


*****************************************************************

    try.
        update zpptetiq
               set status = t_etiq-status
          where werks eq t_etiq-werks and
              nr_etiq eq t_etiq-nr_etiq.
      catch cx_sy_dynamic_osql_error.
         message e001(zpp) with
            'Erro alteração registro. Etiq:' t_etiq-nr_etiq.
    endtry.

*****************************************************************
Qdo alterar linhas em table control, atualizar tabela interna, no PAI.


process after input.

  loop.
    module m_atualiza_linhas.
  endloop.

  module user_command_9000.

module m_atualiza_linhas input.

  MODIFY T_etiq
    FROM T_etiq
    INDEX tc9000-CURRENT_LINE.

endmodule.

*****************************************************************
List box em report

type-pools: vrm.

parameter p_stat(10) as listbox visible length 15.

data: name  type vrm_id,
      list  type vrm_values,
      value like line of list.

  clear: list, list[].
  p_stat = '2'.

  name = 'P_STAT'.
  value-key = '0'.
  value-text = 'Avisos impressos'.
  append value to list.

  value-key = '1'.
  value-text = 'Avisos não impressos'.
  append value to list.

  value-key = '2'.
  value-text = 'Todos'.
  append value to list.

  call function 'VRM_SET_VALUES'
    exporting
      id     = name
      values = list.

---------------
List box em online
listbox

No Include TOP

TYPE-POOLS VRM. 
DATA: LIST          TYPE VRM_VALUES. 
DATA: VALUE         LIKE LINE OF LIST. 

No PBO

 Um module na ultima posição do PBO
 * Exibe os model numbers da ordem 
  MODULE CARREGA_LIST. 

MODULE CARREGA_LIST OUTPUT. 

  CLEAR: LIST, LIST[]. 

  IF NOT E_STATUS[] IS INITIAL. 
    LOOP AT E_STATUS. 
      MOVE: E_STATUS-MATNR TO VALUE-KEY. 
      COLLECT VALUE INTO LIST. 
    ENDLOOP. 

    SORT LIST BY TEXT. 

    CALL FUNCTION 'VRM_SET_VALUES' 
      EXPORTING 
        ID              = 'VBAP-MATNR' 
        VALUES          = LIST 
      EXCEPTIONS 
        ID_ILLEGAL_NAME = 1 
        OTHERS          = 2. 

    IF SY-SUBRC <> 0. 
      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO 
              WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4. 
    ENDIF. 
  ENDIF. 

ENDMODULE.

*****************************************************************

Appendando linhas em uma tabela interna, através de outra tab interna

   append lines of it_aux to it_lista6.


*****************************************************************

data: it_matnr  like range of mara-matnr with header line.

*****************************************************************
A mensagem vai aparecer como erro, mas nao vai desabilitar campos para edição.
MESSAGE s(001) DISPLAY LIKE 'E'.

usar mensagem tipo warning e informativas dentro da field exit. Você precisará enganar o SAP para tal.
Use o seguinte comando:

MESSAGE S999(ZMM) DISPLAY LIKE 'W' WITH text-001.

*****************************************************************
    "Substitui os '.' por espaço
    replace all occurrences of '.' in t_mseg-sgtxt with '' .
    "Substitui os ',' por '.'
    replace all occurrences of ',' in t_mseg-sgtxt with '.' .

    try.
        t_log-betrg = t_mseg-sgtxt.
      catch cx_sy_conversion_overflow.
        clear t_log-betrg.
        message i000(zhrd) with 'Atenção!! Valor vindo do docto material (mseg-txt),' 'está demasiadamente grande.'
                                'Sairá zerado no relatório. Docto' p0015-zuord.
      catch cx_sy_conversion_no_number.
        clear t_log-betrg.
        message i000(zhrd) with 'Atenção!! Valor incoerente vindo do docto material (mseg-txt),' 'está com letra(s).'
                                'Sairá zerado no relatório. Docto' p0015-zuord.
    endtry.

-----------
Convertendo campo flutuante.

    write wa_s076-absat to v_char decimals 3 exponent 0.
    replace ',' with '.' into v_char.
    v_vr_aux = v_char.

*****************************************************************
Para executar transações, sem autorização, coloque um break, na função:

TRANSACTION_CALL.

*****************************************************************
Um exemplo de uma função Z que modifica (atualiza) NF
    (Usa a função 'J_1B_NF_DOCUMENT_UPDATE' in update task)

function zsd_atualiza_msg_nf.
*"----------------------------------------------------------------------
*"*"Módulo função atualização:
*"
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_DOCNUM) LIKE  J_1BNFDOC-DOCNUM
*"  TABLES
*"      T_J_1BNFFTX STRUCTURE  J_1BNFFTX
*"----------------------------------------------------------------------
  tables: ztmp.

  data: v_subrc(1).

  call function 'J_1B_NF_DOCUMENT_READ'
    exporting
      doc_number         = i_docnum
    importing
      doc_header         = wk_header
    tables
      doc_partner        = y_1bnfnad
      doc_item           = y_1bnflin
      doc_item_tax       = y_1bnfstx
      doc_header_msg     = y_1bnfftx
      doc_refer_msg      = y_1bnfref
      doc_ot_partner     = y_1bnfcpd
    exceptions
      document_not_found = 1
      docum_lock         = 2
      others             = 3.

  if sy-subrc ne 0.
    ztmp-mandt = sy-mandt.
    ztmp-msg   = 'Não passou ZSD_ATUALIZA_MSG_NF'.
    ztmp-msg1  = i_docnum.
    modify ztmp.
    exit.
  endif.

*  check sy-subrc = 0.

  ztmp-mandt = sy-mandt.
  ztmp-msg   = 'Após J_1B_NF_DOCUMENT_READ'.
  ztmp-msg1  = i_docnum.
  modify ztmp.

  loop at y_1bnfnad.
    move-corresponding y_1bnfnad to t_partner.
    append t_partner.
  endloop.

  loop at y_1bnflin.
    move-corresponding y_1bnflin to t_item.
    append t_item.
  endloop.

  loop at y_1bnfstx.
    move-corresponding y_1bnfstx to t_item_tax.
    append t_item_tax.
  endloop.

  loop at y_1bnfref.
    move-corresponding y_1bnfref to t_refer_msg.
    append t_refer_msg.
  endloop.

  loop at y_1bnfcpd.
    move-corresponding y_1bnfcpd to t_ot_partner.
    append t_ot_partner.
  endloop.

  call function 'J_1B_NF_DOCUMENT_UPDATE' in update task
    exporting
      doc_number            = i_docnum
      doc_header            = wk_header
    tables
      doc_partner           = t_partner
      doc_item              = t_item
      doc_item_tax          = t_item_tax
      doc_header_msg        = t_j_1bnfftx
      doc_refer_msg         = t_refer_msg
      doc_ot_partner        = t_ot_partner
    exceptions
      document_not_found    = 01
      update_problem        = 02
      doc_number_is_initial = 03
      others                = 08.


  ztmp-mandt = sy-mandt.
  ztmp-msg   = 'Após J_1B_NF_DOCUMENT_UPDATE'.
  v_subrc = sy-subrc.
  concatenate 'sy-subrc' v_subrc into ztmp-msg1.
  modify ztmp.

endfunction.

*****************************************************************

"Encontra a posição exata do conteúdo '&v_vr_energia(10.2)&', na linha do texto

    find '&v_vr_energia(10.2)&' in t_tline-tdline
                    match offset v_col_inicio match length v_tamanho.

    check sy-subrc = 0.

    t_tline-tdline+v_col_inicio(v_tamanho) = v_vr_energia_sc.

*****************************************************************

CALL FUNCTION 'PM_PARTNER_READ'
        EXPORTING
          parvw                  = wa_qmsm-parvw
          parnr                  = wa_qmsm-parnr
        IMPORTING
          diadrc_wa              = wa_diadrc
          diadrp_wa              = wa_diadrp
        EXCEPTIONS
          no_valid_parnr         = 1
          no_valid_parnr_ today   = 2
          no_authority           = 3
          parvw_and_nrart_ inital = 4
          OTHERS                 = 5.
 
 
Busquei na visão VIQMAML (Ações (inclusive cabeçalho da nota)) o número da Nota (QMNUM) e o número sequencial da ação (MANUM).
 
Com esses valores, acessei a tabela QMSM (Nota de qualidade - medidas) buscando os valores PARVW (Função do responsável pela medida) e PARNR (Responsável pela medida (nº parceiro).
 
Com esses dois campos (PARVW e PARNR) vc chama a PM_PARTNER_READ.

*****************************************************************
Objetos envolvidos em todos os desenvolvimentos:

Funções standards:
J_1B_NFE_OUT_OK
J_1B_NF_DOCUMENT_NUMB_GET_NEXT ----> pega próximo nr nf/nfe.


SAPLJ_1B_NFE - LJ_1B_NFEF42  xmlr_tab      LINHA 54
FORM                           / CALL_XI


Badis:
  JOB_NAME_SELF: método JOB_NAME. Este método é disparado antes de aparecer a tela de   ‘Parâmetros de impressão background’.
  Este método é disparado para qualquer outra transação ‘standard’.

   BADI_LE_SHIPMENT ---- at save --- VT01N VT02N (validações)

   CL_NFE_PRINT --- implementação ZCL_NFE_PRINT – método FILL_HEADER
   Objetivo: Exportar para memória, nr docto fiscal DOCNUM;
        (neste momento, o documento fiscal ainda não foi criado no banco de dados).

  SD_CIN_LV60AU02 --- implementação ZSD_CIN_LV60AU02 – método EXCISE_INVOICE_CREATE
  Objetivo: Importar da memória, nr docto fiscal DOCNUM, e executar    
        Função (update task) que insere textos Z (sapscript NF) na tabela J_1BNFFTX.

Função ZSD_ATUALIZA_MSG_NF:
 Objetivo: Através do nr docto fiscal importado, insere todos os textos criados em 
           runtime (sapscript NF), na tabela standard J_1BNFFTX.


--------
method IF_EX_CL_NFE_PRINT~FILL_HEADER.

   include ZSD_NFE_MSG_2. " Inclui informações adicionais de Cabeçalho e também Itens + outros acertos

   include ZSD_NFE003. " Acertos gerais

endmethod.
--------------------------
Includes estão na pasta D:\Lacb\Abap\Programas_Reports\novos\Hypermarcas\nfe.


**************************
Badis:

COR03
Ordens de PP: Badi WORKORDER_UPDATE.
Ao Liberar a ordem (PP) de processo: método AT_RELEASE.
Ao criar ou modificar a lista técnica (PP): Badi BOM_UPDATE.

BADI_J_1BSD~SET_SHIPPING_DATA_FOR_NF (faturamento VF01)  ---- > alterar Número total dos volumes da remessa (ANZPK), espécie (SHPUNT), 
                                       Local de expedição recebimento (vstel), Tipo de veículo de transporte (TRATY),
                                       Identificação do transporte (TRAID).


COR1/COR2 (ordem interna) ---> badi WORKORDER_UPDATE, método AT_RELEASE
                                                      BEFORE_UPDATE.

MD41 (cria requisição compra) ----> badi MD_PURREQ_CHANGE

MIRO (validaçoes antes de salvar) -----> Badi INVOICE_UPDATE método CHANGE_AT_SAVE.
                                         EXIT_SAPLMR1M_001.
                        Função MRM_AMOUNT_CHECK (ENHANCEMENT-POINT mrm_amount_check_01 SPOTS es_saplmrmc).

         EXIT_SAPLMR1M_002: User exit para modificações de campos (T_DRSEG_CO) classificação contábil (item da miro)

J_1B_RANGE_NUMBER -----> qdo imprime NF.

----------------------------------------------------------------------
ME41/ME47:
ME_PURCHDOC_POSTED

----------------------------------------------------------------------
ME51N/ME52N
ME_PROCESS_REQ_CUST -- método PROCESS_ACCOUNT (aba classif contábil):

(gf_aktyp = im_req_item->get_activity( )) ---> 'H' criando .... 'V' Modificando 

  data: re_exkn type exkn,
* Busca Classificação Contábil do Item da Requisição
  call method im_account->get_exkn
    receiving
      re_exkn = re_exkn.

  data: im_req_item  type ref to if_purchase_requisition_item,
        re_item      type mereq_item.

  "Busca detalhes do item da requisição
  call method im_account_ref->get_item
    receiving
      re_item = im_req_item.

  re_item = im_req_item->get_data( ).
  if ( re_item-knttp eq 'A' ) and ( not re_exkn-anln1 is initial ).


E_PROCESS_REQ_CUST~CHECK:
  TYPE-POOLS: abap.

  TYPES: BEGIN OF ty_id,
           id TYPE thead-tdid,
         END OF ty_id.

  DATA: it_items        TYPE          mmpur_requisition_items,
        it_id            TYPE TABLE OF ty_id,
        r_bsart_ser      TYPE RANGE OF eban-bsart,
        wa_item          TYPE          mmpur_requisition_item,
        wa_item_detail   TYPE          mereq_item,
        wa_header_detail TYPE          mereq_header,
        l_valor          TYPE          ztge_constantes-valor,
        l_name           TYPE          thead-tdname,
        lt_texttypes     TYPE          mmpur_texttypes,
        lt_textlines     TYPE          mmpur_t_textlines,
        lt_alllines      TYPE          mmpur_t_textlines,
        l_formatted      TYPE          mmpur_bool,
        l_entries        TYPE          sytfill,
        wa_id            TYPE          ty_id.

  " Include para inserir mensagens.
  INCLUDE mm_messages_mac.

  " Recuperar o cabecalho da requisicao de compras.
  wa_header_detail = im_header->get_data( ).
  " Recuperar itens da requisicao de compras.
  it_items = im_header->get_items( ).

  lt_texttypes-tdobject = 'EBAN'.
  lt_texttypes-tdtext   = 'Texto do item'.
  lt_texttypes-textflag = abap_true.

  LOOP AT it_items INTO wa_item.

    " Recuperar detalhe do item da req.
    wa_item_detail = wa_item-item->get_data( ).

    " Se for requisicao de compra do tipo servico, validar se o texto do item está vazio.
    IF wa_item_detail-bsart IN r_bsart_ser AND NOT r_bsart_ser[] IS INITIAL.

      REFRESH: lt_alllines.

      " Verificar se os textos estão preenchidos.
      LOOP AT it_id INTO wa_id.

        REFRESH: lt_textlines.

        lt_texttypes-tdid = wa_id-id.

        " Buscar os textos que devem ser validados
        wa_item-item->if_longtexts_mm~get_text(
                  EXPORTING im_tdid           = lt_texttypes-tdid
                            im_texttype       = lt_texttypes
                  IMPORTING ex_textlines      = lt_textlines
                            ex_text_formatted = l_formatted ).

        APPEND LINES OF lt_textlines TO lt_alllines.

      ENDLOOP.

      " Se retornou zero linhas, significa que o texto nao foi preenchido.
      IF lt_alllines[] IS INITIAL.

        ch_failed = abap_true.

        " Mensagem de erro.
        mmpur_message_forced 'E'
                             'ZMM'
                             '236'
                             wa_item_detail-bnfpo
                             space
                             space
                             space.
        EXIT.
      ENDIF.

    ENDIF.

  ENDLOOP.

--------------------------

ME21N/ME22N ---- badi ME_PROCESS_PO_CUST método PROCESS_ITEM
Campo trtyp (V - mnodificando)

  data: wa_item  type mepoitem,
        t_tkomv      type                   mmpur_tkomv.

  wa_item = im_item->get_data( ).

      wa_item-bwtar = 'ZINTERNO09'.
      call method im_item->set_data
        exporting
          im_data = wa_item.

      " Carregar condicoes da pricing.
      call method im_item->get_conditions
        importing
          ex_conditions = t_tkomv.


method IF_EX_ME_PROCESS_PO_CUST~PROCESS_HEADER.

 DATA: et_items  TYPE  purchase_order_items,
       et_item   TYPE  purchase_order_item,
       et_item2  TYPE  mepoitem,
       v_mwskz   TYPE  ekpo-mwskz.

et_items = im_header->get_items( ).

*Primeiro imposto IVA
  LOOP AT et_items INTO et_item.
    et_item2 = et_item-item->get_data( ).

    IF NOT et_item2-mwskz IS INITIAL.
      MOVE: et_item2-mwskz TO v_mwskz.
      EXIT.
    ENDIF.
  ENDLOOP.

*Carrega o imposto para campos iniciais.
  LOOP AT et_items INTO et_item.
    et_item2 = et_item-item->get_data( ).

    IF et_item2-mwskz IS INITIAL.
      MOVE: v_mwskz TO et_item2-mwskz.

      CALL METHOD et_item-item->set_data
        EXPORTING
          im_data = et_item2.

    ENDIF.
  ENDLOOP.

endmethod.

Para carregar a grid de itens (assign) ekpo: (SAPLMEPO)POT[].

----------------------------------------------------------------------
MB_CHECK_LINE_BADI
MB_MIGO_BADI (migo)
MB_MIGO_ITEM_BADI
----------------------------------------------------------------------
CTS_REQUEST_CHECK ----> Validações antes de criar request (método CHECK_BEFORE_CREATION).
method IF_EX_CTS_REQUEST_CHECK~CHECK_BEFORE_CREATION.
*> Hypermarcas (Luismar) - 05/08/2008 - chamado NBU-891107 - Início
  "Checa se a descrição da request está conforme padrão determinado.
  include ZCTS_REQUEST_CHECK_001.
*> Hypermarcas (Luismar) - 05/08/2008 - chamado NBU-891107 - Fim
endmethod.

----------------------------------------------------------------------
method if_ex_cts_request_check~check_before_release.

  "Checar 'commit' em código fonte.
  data: v_subrc  type sy-subrc,
        v_user   type user.
  clear v_subrc.
  call function 'ZCHECA_COMMIT'
    exporting
      i_request = request
    importing
      e_subrc   = v_subrc.

  if v_subrc = '4'. "Se encontrou a indicação 'commit' em objetos
    raise cancel.
  endif.

  data: wa_objects  type e071,
        v_objeto    type zbct018-objeto.

  loop at objects into wa_objects.

    "Luismar (24/04/2012) SS 291477 ----------------------------------------------- Início
    "Se o objeto a liberar, é do tipo 'objeto de autorização' (SUSO),
    "enviar email informando que houve criação do objeto de autorização na SU21.
    if ( type eq 'K' or type eq 'W' )         "Se request 'Mãe'..
         and ( wa_objects-object = 'SUSO' ).  "tipo 'objeto de autorização'
      call function 'ZPERF_EMAIL_OBJ' starting new task 'ZSU21'
        exporting
          i_obj_name = wa_objects-obj_name.
    endif.
    "Luismar (24/04/2012) SS 291477 ----------------------------------------------- Fim

    select single objeto into v_objeto
                  from zbct018 where objeto = wa_objects-obj_name.

    if sy-subrc = 0.
      delete from zbct018 where objeto = wa_objects-obj_name.
    endif.

  endloop.
*> Hypermarcas (Luismar) - 08/06/2009 - Fim

  "! teste para criacao de projetos na request
  data: wa_attributes type line of trattributes.

  " Carrega os usuarios do projeto
  select single as4user into v_user
                  from zbct019
                  where as4user = owner.
  if sy-subrc eq 0.
    "Somente para Requests Mãe
    if type eq 'K' or type eq 'W'.
      "Checa se existe Projeto
      read table attributes with key attribute = 'SAP_CTS_PROJECT' into wa_attributes.
      if sy-subrc is not initial.
        message i000(zsd) with 'Favor informar o Projeto'
                               'antes de liberar a request'.
        raise cancel.
      endif.
    endif.
  endif.

  "MFIUZA - 22.03.2012 - P3683 - Inicio

  data: v_msg      type char100,
*        v_obj_name type char30.
        v_obj_name type trobj_name.

  "Executa função que verifica se o objeto o qual se deseja liberar a request,
  "possui request que ainda não subiu pra PRODUÇÃO.
  "Só para Enhancement ou métodos...
  loop at objects into wa_objects
                  where ( object = 'ENHO' ) or ( object = 'METH' ).

    clear v_msg.
    v_obj_name =  wa_objects-obj_name.
    call function 'ZFBC_CHECA_OBJ_BLOQ'
      exporting
        i_obj_name = v_obj_name
        i_request  = request
      importing
        e_msg      = v_msg.

    if not v_msg is initial.
      "Possui request que ainda não subiu pra PRODUÇÃO.
      "message i000(zbc) with v_msg(26) v_msg+26(40).
      raise cancel.
    endif.

  endloop.

  "MFIUZA - 22.03.2012 - P3683 - Fim

endmethod.

------------------ função zperf_email_obj
function zperf_email_obj.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_OBJ_NAME) TYPE  TROBJ_NAME
*"----------------------------------------------------------------------

*----------------------------------------------------------------------*
* Responsável ...: Luismar A. C. Borges                                *
* Data desenv ...: 18/04/2012                                          *
* Solicitante ...: Marta (Perfil)                                      *
* Chamado .......: 291477                                              *
* Objetivo ......: Enviar email informando que houve criação de objeto *
*                  de autorização na SU21.                             *
*----------------------------------------------------------------------*

  "Busca dados do objeto de autorização...
  select single a~objct a~fiel1 a~fiel2 a~fiel3 a~fiel4 a~fiel5 a~fiel6 a~fiel7
                a~fiel8 a~fiel9 a~fiel0 a~oclss a~bname a~fblock a~conversion
                b~ttext
     into wa_obj
          from tobj as a
     inner join tobjt as b
           on b~object = a~objct
     where a~objct = i_obj_name
       and b~langu = sy-langu.

  check sy-subrc = 0.

  "Busca destinatários do email, na tabela de constantes (ZTGE_CONSTANTES)...
  select valor into table t_emails
    from ztge_constantes
   where programa  eq 'SU21_EMAIL'.

  "Verifica se encontrou email(s) cadastrados...
  check not t_emails[] is initial.

  clear:   it_message[], it_receivers[], gd_doc_data, gd_sent_all, it_packing_list[].
  refresh: it_message[], it_receivers[], it_packing_list[].

  "Preenche tabela interna com destinatários...
  loop at t_emails.
    it_receivers-receiver = t_emails-email.
    it_receivers-rec_type = 'U'.
    append it_receivers.
  endloop.

  "Assunto do email...
  gd_doc_data-obj_descr = 'Objeto criado na SU21'.
  gd_doc_data-doc_size  = 1.
  gd_doc_data-obj_langu = sy-langu.
  gd_doc_data-obj_name  = 'SAPRPT'.
  gd_doc_data-sensitivty = 'F'.

  "Mensagens corpo do email ----------------------------------------------- Início
  concatenate 'Objeto:' i_obj_name '-' wa_obj-ttext
               into it_message separated by space.
  append it_message.

  clear v_ctext.
  "Descrição da classe do objeto de autorização...
  select single ctext into v_ctext
     from tobct
     where langu = sy-langu
       and oclss = wa_obj-oclss.

  concatenate 'Classe:' wa_obj-oclss '-' v_ctext
               into it_message separated by space.
  append it_message.

  clear it_message.
  append it_message.

  it_message =  'Campo(s) de autorização:'.
  append it_message.

  "Campo(s) de autorização...
  do 10 times.

    v_index = sy-index.
    if sy-index = 10.
       v_index = '0'.
    endif.

    concatenate 'WA_OBJ-FIEL' v_index into v_campo.
    assign (v_campo) to <fs_campo>.
    check <fs_campo> is assigned.
    check not <fs_campo> is initial.

    "Descrição do campo de autorização (elemento de dados)...
    clear v_ddtext.
    select single ddtext into v_ddtext
           from dd04t
                where rollname = <fs_campo>
                and ddlanguage = sy-langu.

    clear it_message.
    concatenate <fs_campo> '-' v_ddtext
               into it_message separated by space.
    append it_message.
  enddo.

  clear it_message.
  append it_message.

  it_message = 'Email disparado automaticamente. Favor não responder.'.
  append it_message.
  "Mensagens corpo do email ----------------------------------------------- Fim

  "Quantidade de linhas do corpo do email.
  it_packing_list-transf_bin = space.
  it_packing_list-head_start = 1.
  it_packing_list-head_num   = 0.
  it_packing_list-body_start = 1.
  describe table it_message lines it_packing_list-body_num.
  it_packing_list-doc_type = 'RAW'.
  append it_packing_list.

  "Enviar email...
  clear gd_sent_all.
  call function 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    exporting
      document_data              = gd_doc_data
      put_in_outbox              = 'X'
      commit_work                = 'X'
    importing
      sent_to_all                = gd_sent_all
    tables
      packing_list               = it_packing_list
      contents_txt               = it_message
      receivers                  = it_receivers
    exceptions
      too_many_receivers         = 1
      document_not_sent          = 2
      document_type_not_exist    = 3
      operation_no_authorization = 4
      parameter_error            = 5
      x_error                    = 6
      enqueue_error              = 7
      others                     = 8.
  "Enviar email, avisando da criação do objeto de autorização -------------------------- Fim

endfunction.
----------------------------------------------------------------------
tobj --- (contém os campos do objeto de autorização)
USOBT_C --- (Relação transação --> obj.autor.(cliente))

----------------------------------------------------------------------

VL02N ---- DELIVERY_PUBLISH --~PUBLISH_AFTER_SAVE ---- qdo registrar saída de mercadorias.
           LE_SHP_DELIVERY_PROC --- validações (cabeçalho/itens), alterações de valores.

           LE_SHP_GOODSMOVEMENT ----> Aqui pode-se alterar:
           Campos da estrutura cabeçalho documento material. Ex: cs_imkpf-budat = l_fkdat.

           Campos da estrutura XLIKP (cabeç fornecimento).
           assign (c_assign_xlikp_wadat_ist) to <fs_wadat_ist>.
           if <fs_wadat_ist> is ASSIGNED.
             move l_fkdat to <fs_wadat_ist>.
             unassign <fs_wadat_ist>.
           endif.

           "Campos da estrutura LIKP (cabeç fornecimento).
           assign (c_assign_likp_wadat_ist) to <fs_wadat_ist>.
           if <fs_wadat_ist> is ASSIGNED.
             move l_fkdat to <fs_wadat_ist>.
             unassign <fs_wadat_ist>.
           endif.

           Badi CFOP_DET_PREP, método CHANGE_PARAMETERS_SD: Altera estrutura J_1BINCFOP (Interface: determinação de CFOP)
                                                            no momento de determinar CFOP.

Tabela SHP_IDX_GDSI (Entregas: saída de mercadorias ainda não registrada)
------------------------------------------------------------------------------

ME21N/ME22N ---- ME_PURCHDOC_POSTED  --- pedidos de compra (após salvar).
Para carregar a grid de itens (assign) ekpo: (SAPLMEPO)POT[].

Campo trtyp (V - mnodificando)

BADI_SCD_SAVE --- custo de frete (vi01).  ---- Início
  TYPES:
  BEGIN OF ty_mseg,
    ebeln TYPE ekbe-ebeln,
    ebelp TYPE ekbe-ebelp,
    bewtp TYPE ekbe-bewtp,
    mblnr TYPE mseg-mblnr,
    mjahr TYPE mseg-mjahr,
    zeile TYPE mseg-zeile,
    smbln TYPE mseg-smbln,
    xblnr TYPE mkpf-xblnr,
  END OF ty_mseg,

  BEGIN OF ty_lips,
    vbeln TYPE lips-vbeln,
    posnr TYPE lips-posnr,
    vgbel TYPE lips-vgbel,
    vgpos TYPE lips-vgpos,
    verur TYPE likp-verur,
    ebeln TYPE ekbe-ebeln,
    ebelp TYPE ekbe-ebelp,
  END OF ty_lips.

  DATA: BEGIN OF w_ekes,
          ebeln           TYPE ekes-ebeln,
          ebelp           TYPE ekes-ebelp,
          vbeln           TYPE ekes-vbeln,
          vbelp           TYPE ekes-vbelp,
          xblnr           TYPE ekes-xblnr,
          menge           TYPE ekes-menge,
          dabmg           TYPE ekes-dabmg,
          estkz           TYPE ekes-estkz,
        END   OF w_ekes.

  DATA:
    t_item_x   TYPE STANDARD TABLE OF v54a0_scd_item, " Atual
    t_item_y   TYPE STANDARD TABLE OF v54a0_scd_item, " Anterior
    t_lips     TYPE STANDARD TABLE OF ty_lips,
    t_mseg     TYPE TABLE          OF ty_mseg,
    t_mseg_est TYPE TABLE OF          ty_mseg,
    w_mseg_est TYPE                   ty_mseg,
    w_mseg     TYPE                   ty_mseg,
    t_vfsi    TYPE  TABLE OF vfsivb,
    wa_lips   TYPE           ty_lips,
    wa_vfsi   TYPE           vfsivb,
    l_mseg    TYPE c.

  DATA: t_ekes            LIKE STANDARD TABLE OF w_ekes.

  IF sy-tcode EQ 'VI02'.

    LOOP AT t_item_y INTO w_item_y.
      APPEND LINES OF w_item_y-vfsi TO t_vfsi.
    ENDLOOP.

    " Ler dados da MSEG amanha.
    FREE: t_mseg.

    CHECK NOT t_vfsi[] IS INITIAL.
    SELECT lips~vbeln lips~posnr lips~vgbel lips~vgpos
           likp~verur
      INTO TABLE t_lips
      FROM lips
      INNER JOIN likp ON likp~vbeln EQ lips~vbeln
      FOR ALL entries IN t_vfsi
      WHERE lips~vbeln EQ t_vfsi-vbeln AND
            lips~posnr EQ t_vfsi-posnr.

    CHECK NOT t_lips[] IS INITIAL.

    LOOP AT t_lips INTO wa_lips.
      wa_lips-ebeln = wa_lips-vgbel.
      wa_lips-ebelp = wa_lips-vgpos.
      MODIFY t_lips FROM wa_lips INDEX sy-tabix.
    ENDLOOP.

*> Busca valores do aviso de recebimento e documento de compra da
*> tabela de confirmações do pedido.
    SELECT ebeln ebelp vbeln vbelp xblnr menge dabmg estkz
      FROM ekes
      INTO TABLE t_ekes
       FOR ALL ENTRIES IN t_lips
     WHERE ( ebeln EQ t_lips-ebeln ) "pedido
       AND ( ebelp EQ t_lips-ebelp )
       AND ( vbeln EQ t_lips-vbeln ) "aviso de recebimento
       AND ( vbelp EQ t_lips-posnr ).

    DELETE t_ekes WHERE ( estkz NE '2' ). "somente aviso de recebimento

    CHECK ( NOT t_ekes[] IS INITIAL ).

    SORT t_lips BY vbeln posnr.
    SORT t_ekes BY ebeln ebelp vbeln vbelp.

    LOOP AT t_vfsi INTO wa_vfsi.

      LOOP AT t_lips INTO wa_lips
                    WHERE ( vbeln EQ wa_vfsi-vbeln )
                      AND ( posnr EQ wa_vfsi-posnr ).

        READ TABLE t_ekes INTO w_ekes
                      WITH KEY ebeln = wa_lips-ebeln "Pedido
                               ebelp = wa_lips-ebelp
                               vbeln = wa_lips-vbeln "Aviso de recebimento/entrega
                               vbelp = wa_lips-posnr BINARY SEARCH.

        CHECK ( sy-subrc EQ 0 ).

        " Se for a mesma nota e a quantidade confirmada for igual a reduzida
        CHECK ( wa_lips-verur EQ w_ekes-xblnr ) AND
              ( w_ekes-menge  EQ w_ekes-dabmg ) AND
              ( NOT w_ekes-menge  IS INITIAL  ).

        l_mseg = 'X'.
        MESSAGE i899(mm) WITH 'Cancelamento impossível. Estornar primeiro a MIGO.' RAISING error.
        EXIT.

      ENDLOOP.
      IF NOT l_mseg IS INITIAL.
        EXIT.
      ENDIF.

    ENDLOOP.
  ENDIF.


BADI_SCD_SAVE --- custo de frete (vi01).  ---- Fim

*****************************************************************
User exit  userexit enhancement point

---------
EXIT_SAPLSUSF_001 (disparado no momento que loga no R3. Na primeira tela do SAP, depois do login)

---------
VT01 / VT02
EXIT_SAPLV56U_004
---------
COR1 / CO01 ---> EXIT_SAPLCOZV_001 (validações).

                 EXIT_SAPLV01Z_002 (alterar lote).

                 Se possuir mais de uma área de avaliação (popup onde usuário seleciona 1):
                 No Módulo de função: VB_CREATE_BATCH
                 ENHANCEMENT-POINT vb_create_batch_02 SPOTS es_saplv01z.  Codificação:
  "Luismar (11/10/2011) SS 227325 - Início
  "Na transação CO01, qdo material e centro possui mais de 1 (um) tipo de avaliação...
  "alterar lote qdo usuário seleciona tipo avaliação.
  if ( sy-batch is initial )                "execução online...
       and ( sy-binpt is initial )          "não é batch input...
           and ( l_idoc_call is initial )   "não é execução de IDOC...
              and ( t149-kzbaa IS INITIAL ) "X = tipo de avaliação será definido automaticamente...
                  and ( lin > 1 ).          "mais de 1 (um) tipo de avaliação...
      data: wa_bncom             type bncom.
      field-symbols: <fs_bncom>  type bncom.

      assign ('(SAPLV01Z)X_BNCOM') to <fs_bncom>.
      if <fs_bncom> is assigned.
         wa_bncom = <fs_bncom>.
         call function 'ZFCO_ALTERA_LOTE'
           exporting
             x_bncom                    = wa_bncom
             i_bwtar                    = ymcha-bwtar
           importing
             new_charg                  = ymcha-charg.

         move ymcha-charg to: MCHB_KEY-CHARG,    "SS 235662
                              MCH1_KEY-CHARG,    "SS 235662
                              MCHA_KEY-CHARG.    "SS 235662
      endif.
  endif.
  "Luismar (11/10/2011) SS 227325 - Fim

---------
VI01/VI02 ----> EXIT_SAPLV54U_004 (include ZXV54U07).

Tabela que contém enhancements: ENHHEADER  / ENHINCINX

SE38 ----> EXIT_SAPLS38E_001.

-----------------------------------------------------------------
ME21N/ME22
Campo I_TRTYP ou VC_AKTYP (V - mnodificando)

ZXM06U44 (EXIT_SAPMM06E_013) - No momento de salvar. É disparada na última estapa do 
processo, onde o pedido está ok e com o número gerado.
ZXM06U43 (EXIT_SAPMM06E_012)- No momento de verificar/salvar. É disparada no processo 
anterior ao de cima, onde o pedido ainda será verificado. Neste 
momento não possui ainda o número do pedido gerado.

Para carregar a grid de itens (assign) ekpo: (SAPLMEPO)POT[].

EXIT_SAPLMLSP_030  ----- aba 'Serviços'

ME21N/ME22N - EXIT_SAPLEBND_002 (Estratégia de Liberação).
ME51N/ME52N - EXIT_SAPLEBND_001 (Estratégia de Liberação).
              EXIT_SAPLMEREQ_005 (verificações, msg erro).            
              DATA: gf_aktyp TYPE aktyp.
              "gf_aktyp = im_req_item->get_activity( ).    H - criando     V - Modificando

                         (aba 'serviços' if im_data_new-pstyp = '9')
                   assign ('(SAPLMLSK)COBL-ANLN1') to <fs_anln1>.
                   assign ('(SAPLMLSK)COBL-ANLN2') to <fs_anln2>.
                   if <fs_anln1> is assigned.
                      v_anln1 = <fs_anln1>.
                      unassign <fs_anln1>.
                   endif.
                   if <fs_anln2> is assigned.
                      v_anln2 = <fs_anln2>.
                      unassign <fs_anln2>.
                   endif.

              EXIT_SAPLMEREQ_003:
                 DATA: ls_mereq_item TYPE mereq_item.

                 IF NOT im_req_item IS INITIAL.
                    ls_mereq_item = im_req_item->get_data( ).
                 endif.

                 "set new item data to system
                  CALL METHOD im_req_item->set_data( ls_mereq_item ).

              EXIT_SAPLMEREQ_010:
                clear wa_messages.
                wa_messages-type       = 'E'.
                wa_messages-id         = 'ZMM'.
                wa_messages-number     = '152'.
                append wa_messages to ex_messages.

"Buscar dados de pedidos de compra (mnarcar flags de busca no 'exporting':
call function 'BAPI_PO_GETDETAIL'
exporting
purchaseorder = <fs_ekko>-ebeln
items = 'X'              "ler ekpo
history = 'X'            "ler ekbe (histórico de recebimentos)
tables
po_items = lt_item_bapi
po_item_history = lt_hist_bapi
po_item_history_totals = lt_histt_bapi
return = lt_return.



-------------------------------------------------
TABELAS DE ESTRATÉGIA:
T16FS
T16FV


ML81N ---- EXIT_SAPLEBND_003    (Estratégia de Liberação).

        " define estratégia
        CALL FUNCTION 'ME_REL_STRATEGIE_EKKO'
          EXPORTING
            i_cekko_new   = wa_cekko
          IMPORTING
            e_frggr       = vl_frggr
            e_frgst       = vl_frgst
          EXCEPTIONS
            error_message = 1
            OTHERS        = 2.

ME41/ME47:
EXIT_SAPMM06E_012

-----------------------------------------------------------------
VF01 -----> EXIT_SAPLV60B_001 ---- (validações) (preencher o XBLNR)
            EXIT_SAPLV60B_006 ---- redeterminação de conta contábil
            EXIT_SAPLV60B_008 ---- (modifica dados contáveis ---- meins, matnr, etc)

VL01N/VL02N/VL0? ---> MV50AFZ1 - FORM USEREXIT_SAVE_DOCUMENT_PREPARE.

Alterar o código de imposto (e_lips-j_1btxsdc), na VL01N (criação fornecimento), para modificar o CFOP:
VL01N (ENHANCEMENT-POINT determine_localised_data_01 SPOTS es_saplj1bk.), include LJ1BKF01.


SD/FI - ACCIT - EXIT_SAPLV60B_004, EXIT_SAPLV60B_008.

EXIT_SAPLACC4_001 (altera dados cabeçalho e itens contábeis).

VA01 -----> MV45AFZB (form userexit_move_field_to_cobl).
call_bapi (variável q indica que processo foi chamado por bapi)

            FORM userexit_new_pricing_vbap CHANGING new_pricing.
            IF new_pricing IS INITIAL.
               FIELD-SYMBOLS: <fs_j_1btxsdc>.
               ASSIGN ('(SAPMV45A)VBAP-J_1BTXSDC') TO <fs_j_1btxsdc>.
               IF sy-subrc EQ 0.
                  IF <fs_j_1btxsdc> NE *vbap-j_1btxsdc.
                     new_pricing = 'C'.
                  ENDIF.
               ENDIF.
            ENDIF.

            Alterar valores de conditions.
            RV61AFZB
            FORM USEREXIT_XKOMV_BEWERTEN_END
            tabela XKOMV. KOMK.

            LV69AFZZ (form userexit_field_modification):
            Alterar atributos da tela de conditions (habilitar/desabilitar) campos.
            Ex:
               if XKOMV-KSCHL = 'ZVLD'
                  if sy-tcode = 'VA01' or sy-tcode = 'VA02'. 
                     if screen-name = 'KOMV-KBETR'. 
                        screen-input = 0. 
                     endif. 
                  endif.
               endif.


            EXIT_SAPMV45A_003 (antes do commit).

            MV45AFZA (neste caso, testar o botão pressionado --- FCODE = 'SICH').
            Form USEREXIT_REFRESH_DOCUMENT.

The field T180-TRTYP has the value 'H' in creation mode, the value
'V' in change mode and the value 'A' in display mode.

CALL_FUNCTION = SPACE (nâo processado por chamada de função (bapi)).


VA01/VA02 ------ iNCLUDE MV45AF0B_BELEG_SICHERN ----- 
          enhancement-point sapmv45a_21 spots es_sapmv45a.
          antes do 'commit' salvar a ordem (antes do 'call function RV_SALES_DOCUMENT_UPDATE')

call_bapi (variável q indica que processo foi chamado por bapi)

---------------------
Validar textos digitados na ordem de venda (MV45AFZZ -- FORM USEREXIT_SAVE_DOCUMENT_PREPARE)...

    loop at xthead where tdspras = 'P'
                       and updkz ne 'D'.

      clear table_zsdt001-valor.

      "O tamanho máximo do texto tem atribuição registrada na tabela de parametrização?
      read table table_zsdt001 with key programa = xthead-tdobject
                                    tipo_reg = xthead-tdid binary search.

      check ( sy-subrc = 0 ) and ( not table_zsdt001-valor is initial ).

      "Busca o texto digitado na ordem de venda...
      refresh t_tline[].
      call function 'READ_TEXT'
        exporting
          id       = xthead-tdid
          language = xthead-tdspras
          name     = xthead-tdname
          object   = xthead-tdobject
        importing
          header   = v_thead
        tables
          lines    = t_tline.

      check not t_tline[] is initial.

      clear v_bytes.
      "Verifica a quantidade de caracteres inseridos no texto...
      loop at t_tline into e_tline.
        v_bytes = v_bytes + strlen( e_tline-tdline ).
      endloop.

      if v_bytes > table_zsdt001-valor.
        "Se qtde de caracteres digitados ultrapassar o limite permitido... mensagem de erro.
        message e000(zsd) with  'Texto: ' xthead-tdtext
                                '- Tamanho máximo excedido: ' table_zsdt001-valor.
      endif.
    endloop.
---------------------

VF01 -----> Alterar valores de conditions.
            RV60AFZZ
            FORM USEREXIT_PRICING_PREPARE_TKOMK.
            RV60AFZC (altera condição de pagamento) - XVBRK.

MM01/MM02 EXIT_SAPLMGMU_001 -- salvar materiais.

XD01/XD02 EXIT_SAPMF02D_001 -- cadastro cliente.
T020-AKTYP = 'H'    ....... criando
T020-AKTYP = 'V'.   ....... modificando

XK01/XK02 EXIT_SAPMF02K_001 -- cadastro fornecedor. Exit pra verificação de campos. Mensagens de
                                                       erros podem ser usadas.
T020-AKTYP = 'H'    ....... criando
T020-AKTYP = 'V'.   ....... modificando

-----------------------------------------------------------------

F110 -----> EXIT_SAPMFDTA_001 (no momento de realizar download de proposta já geradas)
            includes ZXDTAU01 e ZXDTAF01 (pasta \lacb\abap\exits\hypermarcas).

Enhancement point:
Z_INTER (include LJ1BGF01) ----> Altera valores na tabela de impostos do documento fiscal.
                                 E tmbm pode alterar outros dados da NF.
ZFORMULA_903 (Include RV50C903) ---> VL31 (recebimento).

--------------------------------------
VT01N:

EXIT_SAPLV56S_001 (alterar etapas VTTS ou parceiros transporte VTPA)
EXIT_SAPLV56U_004 -- ZXV56U11 (ZXV56U11_02 (ZSHP_IBDLV_CREATE_FROM_OBDLV)) --> No momento que finalizar o transporte (VT01N).
             Cria o Aviso de Recebimento.
             Cria lançamento contábil (substituindo Miro).


 ----- EXIT_SAPLV56F_010:
*"*"Lokale Schnittstelle:
*"       IMPORTING
*"             VALUE(I_TVTK) LIKE  TVTK STRUCTURE  TVTK
*"       TABLES
*"              I_XVTTP STRUCTURE  VTTPVB
*"              C_XVTTS STRUCTURE  VTTSVB
*"              C_YVTTS STRUCTURE  VTTSVB
*"              C_XVTSP STRUCTURE  VTSPVB
*"              C_YVTSP STRUCTURE  VTSPVB
*"              C_XVBPA STRUCTURE  VBPAVB
*"              C_YVBPA STRUCTURE  VBPAVB
*"              C_XVBADR STRUCTURE  SADRVB
*"              I_XTRLK STRUCTURE  VTRLK
*"              I_XTRLP STRUCTURE  VTRLP
*"       CHANGING
*"             REFERENCE(C_XVTTK) LIKE  VTTKVB STRUCTURE  VTTKVB

EXIT_SAPLV56F_012

CALL FUNCTION 'ST_SHIPMENT_LEG_DETERMINATION'
-------------------------------

------------- Objetos importantes NF (preenchimento da J_1bnfdoc e j_1bnflin) ---- Início

J_1B_NF_DOC_INSERT_FROM_OBJECT -- enhanc no começo da função (aqui pode-se mudar todas as tabelas internas. Alterar as gbobj_*) 

SAPLJ1BG                       / LJ1BGF01
FORM                           / FILL_NF_PRINT_CONTROL  (momento que pega o próximo nr nfe)

LJ1BGF01: FORM fill_nf_text_line.  Tabela interna wnfref.

LJ1BIF01 (qdo NF na MIRO) (FORM nf_object_add)  ---->
                                     PERFORM nf_header_create. 
                       *--- Preenhe Items LJ1BFF01 (MM) ---  CALL FUNCTION 'J_1B_NF_IV_SUMMING'
migo ou movimentações de entrada:
SAPLJ1BF                       / LJ1BFF01
FORM                           / CREATE_NOTA_FISCAL

Include LJ1BNF04:
No final do form save_tax_fields (Campos de impostos):

ENHANCEMENT 29  ZMM_DETERMINA_ORIGEM_DESTINO.    "active version
  data: r_bwart type range of ztge_constantes-valor.

  "Luismar (28/02/2012) SS 274468 ------------------------------------------------- Início
     "Valida o processo saída filial fornecedor...
     "Este desenvolvimento visa permitir que a operação entre Filial e Fornecedor,
     "determine corretamente as taxas de imposto.

  if p_mwart = 'V'.    "Tipo de imposto 'IVA suportado'...

     clear r_bwart[].
     refresh r_bwart[].
     "Busca registros (tipos de movimento), na tabela de constantes...
     call function 'ZGE001TF01'
       exporting
         i_programa      = 'ZMM_DETERMINA_ORIGEM_DESTINO'
         i_constantes    = 'BWART'
       tables
         t_valores_const = r_bwart
       exceptions
         const_not_found = 1
         const_empty     = 2
         others          = 3.

     if ( not r_bwart[] is initial )      "se encontrou registro na tabela de constantes...
            and ( p_i_mseg-bwart in r_bwart ). "se tipo de movimento está na tabela de constantes...
         my_taxcom-txreg_sf = my_t001w-regio.  "Move UF do centro para UF Emissor (região fiscal).
         my_taxcom-txreg_st = my_lfa1-regio.   "Move UF do fornecedor para UF Recebedor (região fiscal).

         "Transfere dados para a 'condition'...
         call function 'J_1B_SAVE_TAX_FIELDS'
           exporting
             i_taxcom = my_taxcom.
     endif.

  endif.
  "Luismar (28/02/2012) SS 274468 ------------------------------------------------- Fim

ENDENHANCEMENT.


-----------------------------------------------------------
                                     PERFORM nf_item_create.
                                     ENHANCEMENT-POINT NF_OBJECT_ADD_01 SPOTS ES_SAPLJ1BI. (modificar cabeçalho e item docto fiscal)

------------- Objetos importantes NF (preenchimento da J_1bnfdoc e j_1bnflin) ---- Fim

SAPLV60A                       / LV60AU02
FUNCTION                       / RV_INVOICE_DOCUMENT_ADD 	
linha 754

SAPLV60B                       / LV60BU01                       /
FUNCTION                       / RV_ACCOUNTING_DOCUMENT_CREATE


BADI_SD_ACCOUNTING
EXIT_SAPLV60B_001   (aqui pode-se além de validações (msg erro), pode-se alimentar o campo xblnr)

Outra opção pra alterar o XBLNR: Include LJ1BGF01 (criar uma implementação (enhancemment) no final do form nf_create_objects).FORM nf_object_add.
                                 Modificar a tabela interna wvbrk.

-----
Include LJ1BGF01 (FORM get_branch USING fwerks) ---- Alterar o local de negócios (inclusive dados da j_1bnfdoc (wnfdoc))
ENHANCEMENT 6  OI0_COMMON_SAPLJ1BG.

FORM fill_nf_header_interf (pode-se criar um enhancement aqui tmb).
-----

BADI_SD_ACCOUNTING

BAPI_ACC_EMPLOYEE_REC_POST
BAPI_ACC_EMPLOYEE_PAY_CHECK
BAPI_ACC_EMPLOYEE_EXP_POST
BAPI_ACC_EMPLOYEE_PAY_POST
BAPI_ACC_GOODS_MOVEMENT_POST
BAPI_ACC_INVOICE_RECEIPT_POST
BAPI_ACC_GL_POSTING_POST

FUNCTION EXIT_SAPLACC4_001.
*"----------------------------------------------------------------------
*"*"Lokale Schnittstelle:
*"  TABLES
*"      T_ACCIT STRUCTURE  ACCIT
*"      EXTENSION STRUCTURE  BAPIEXTC
*"      RETURN STRUCTURE  BAPIRET2
*"      T_ACCWT STRUCTURE  ACCIT_WT OPTIONAL
*"  CHANGING
*"     VALUE(T_ACCHD) LIKE  ACCHD STRUCTURE  ACCHD
*"----------------------------------------------------------------------


INCLUDE ZXACCU15 .


ENDFUNCTION.
--------------------------------------------------------
Momento criação docto contábil (XBKPF , XBSEG)

SAPLFACI                       / LFACIU05
FUNCTION                       / FI_DOCUMENT_POST

  call function 'RF_GET_DOCUMENT_NUMBER' (próximo nr docto contábil)

SAPLFACI                       / LFACIF5D
FORM                           / FI_DOCUMENT_PREPARE

Abaixo, o momento onde passam as tabelas de contabilidade, e salva o docto.
SAPLFACI                       / LFACIF57
FORM                           / FI_DOCUMENT_POST
CALL FUNCTION 'POST_DOCUMENT' IN UPDATE TASK
         TABLES
              T_AUSZ1 = XAUSZ1
              T_AUSZ2 = XAUSZ2
              T_AUSZ3 = XAUSZ3                           " note 0522252
              T_BKP1  = XBKP1
              T_BKPF  = XBKPF
              T_BSEC  = XBSEC
              T_BSED  = XBSED
              T_BSEG  = XBSEG
              T_BSEGC = XBSEGC
              T_CCARDEC = XCCARDEC
              T_BSET  = XBSET
              T_BSEU  = XBSEU.


------------------------------
ZFI_BTE_00001130 – Criação de um botão customizado na FB02/FB03.
ZFI_BTE_00001120 – User command a ser processado ao acionar o novo botão.

*----------------------
Modificar docto contábil (fb02):

  DATA: tl_accchg   TYPE STANDARD TABLE OF accchg,
        el_accchg   TYPE accchg.

  DATA: vl_awtyp    TYPE acchd-awtyp,
        vl_awref    TYPE acchd-awref,
        vl_aworg    TYPE acchd-aworg,
        vl_buzei    TYPE bseg-buzei.


  REFRESH: tl_accchg.
  CLEAR:   el_accchg.
  el_accchg-fdname  = 'ZLSPR'.
  el_accchg-oldval  = 'J'.
  el_accchg-newval  = 'Y'.
  APPEND el_accchg TO tl_accchg.

    LOOP AT tl_bseg INTO el_bseg.

*     Cabeçalho - BKPF
      CLEAR: vl_awtyp.
      vl_awtyp = 'BKPF'.

*     Nº doc. contábil / antecipação
      CLEAR: vl_awref.
      vl_awref = el_bseg-belnr.

*     Empresa + Exercicio
      CLEAR: vl_aworg.
      CONCATENATE el_bseg-bukrs
                  el_bseg-gjahr
             INTO vl_aworg.

*     Item doc. contábil / antecipação
      CLEAR: vl_buzei.
      vl_buzei = el_bseg-buzei.

*     Alterar o Tipo de Bloqueio de Pagamentos
*     Y - Forn c/adiant e s/R

      CALL FUNCTION 'FI_DOCUMENT_CHANGE'
        EXPORTING
          i_awtyp              = vl_awtyp
          i_awref              = vl_awref
          i_aworg              = vl_aworg
          i_buzei              = vl_buzei
        TABLES
          t_accchg             = tl_accchg
        EXCEPTIONS
          no_reference         = 1
          no_document          = 2
          many_documents       = 3
          wrong_input          = 4
          overwrite_creditcard = 5
          OTHERS               = 6.

    ENDLOOP. "tl_bseg

Ou, pode passar somente a chave:
  call function 'FI_DOCUMENT_CHANGE'
    exporting
      i_bukrs              = t_bseg-bukrs
      i_belnr              = t_bseg-belnr
      i_gjahr              = t_bseg-gjahr
      i_buzei              = t_bseg-buzei
    tables
      t_accchg             = t_accchg

Obs: Somente atualiza documentos de cliente (D) ou fornecedor (K).

*---------------
VL02N - registrar saída mercadoria (Momento que cria documento material):
SAPFV50W                       / FV50WF0W
FORM                           / WA_MATERIALBELEG_ERZEUGEN

  CALL FUNCTION 'MB_CREATE_GOODS_MOVEMENT'
    EXPORTING
            ...


SAPMV50A                       / FV50XF0B_BELEG_SICHERN
FORM                           / BELEG_SICHERN_POST
(momento antes do commit)
enhancement-section     beleg_sichern_post_21 spots es_fv50xf0b_beleg_sichern include bound.
  if v50agl-no_commit = ' '  and
     v50agl-sofauftrag ne charx.
*   Bei Aufruf aus Verpacken wird der Commit extern gesetzt
    perform sd_monitor_protocol_save.
    if v50agl-synchron ne charx.
      commit work.
    else.
      commit work and wait.
    endif.
  endif.



*---------------
Depois do 'commit' da VT02N (final do form abaixo)
SAPMV56A                       / MV56AF99N_FCODE_ROUTINES
FORM                           / CALL_SAVE
* v_n_864938
* send message
* (after COMMIT WORK to override other messages from somewhere else)
  PERFORM meldung_sicherung.
* ^_n_864938

*---------------
Ponto da Me21n/me22n ----- no commit
SAPLMEPO                       / MM06EF0B_BUCHEN
FORM                           / BUCHEN
linha 1376
CALL FUNCTION 'ME_UPDATE_DOCUMENT' IN UPDATE TASK




*---------------
Ponto da MIGO ----- no commit
LMIGOKG1 - GOODS_MOVEMENT_FILL_04 SPOTS ES_SAPLMIGO.
(procure por commit)

*---------------
Ponto da VA01/VA02 ----- no commit
include MV45AF0B_BELEG_SICHERN
form BELEG_SICHERN (linha 1589)
...      if us_syncron = space.
        commit work.
      else.
        commit work and wait.
      endif.

Ou user exit (após commit e limpeza de estruturas):
MV45AFZA (neste caso, testar o botão pressionado --- FCODE = 'SICH').
Form USEREXIT_REFRESH_DOCUMENT.

-----------------
Alterar telas de pesquisa (match code) na VA01/VA02/VA03 --- search help f4
SAPLV05K                       / LV05KU02
FUNCTION                       / RV_HELP
ENHANCEMENT-POINT RV_HELP_06 SPOTS ES_SAPLV05K.

SHOWTAB[]
------------------------

Redeterminar CFOP:

Funções:
J_1B_SD_CFOP
J_1B_SD_CFOP_REDET
J_1B_SD_SA_CHANGE_VBAP
J_1B_SD_SA_FILL_VBAP
J_1B_NF_CFOP_2_DETERMINATION
-----------------------------------------
Transação J_1BAPNV --------> para determinação de cfops
*****************************************************************
  data: l_prog_tab_local like sy-repid occurs 10 with header line.

  read report l_prog_tab_local into l_abap_source.

Rotinas na função K_KKB_FIELDCAT_MERGE

*****************************************************************
Monta fieldcat a partir de tabela Interna

Campos da t_saida devem ser com 'like'

* Monta fieldcat da tabela informada
  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
    exporting
      i_program_name         = sy-repid
      i_internal_tabname     = p_tabname
      i_inclname             = sy-repid
    changing
      ct_fieldcat            = t_fieldcat
    exceptions
      inconsistent_interface = 1
      program_error          = 2
      others                 = 3.

  delete t_fieldcat where fieldname = 'BUKRS'.

  loop at t_fieldcat into wa_fieldcat.

    clear wa_fieldcat-key.

    case wa_fieldcat-fieldname.

      when 'BUKRS'.
        wa_fieldcat-no_out = c_yes.
      when 'BELNR'.
        wa_fieldcat-hotspot = c_yes.
      when 'SALDO'.
        perform z_texto using text-019   "Saldo em R$
              changing wa_fieldcat-seltext_l.

    endcase.

    modify t_fieldcat from wa_fieldcat.

  endloop.

endform.                    "z_monta_fieldcat

form z_texto  using    p_text
              changing p_texto.

  p_texto = wa_fieldcat-reptext_ddic = wa_fieldcat-seltext_s
          = wa_fieldcat-seltext_m = p_text.

endform.                    " z_texto

---------- para aproveitar e preencher tabela fieldcat LVC
data: gt_fieldcat type lvc_t_fcat,
      wa_fieldcat type lvc_s_fcat,
      gt_fieldcat_aux type slis_t_fieldcat_alv,
      wa_fieldcat_aux type slis_fieldcat_alv.

  "Monta fieldcat da tabela informada
  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
    exporting
      i_program_name         = sy-repid
      i_internal_tabname     = 'T_SAIDA'
      i_inclname             = sy-repid
    changing
      ct_fieldcat            = gt_fieldcat_aux
    exceptions
      inconsistent_interface = 1
      program_error          = 2
      others                 = 3.


  loop at gt_fieldcat_aux into wa_fieldcat_aux.

    MOVE-CORRESPONDING wa_fieldcat_aux to wa_fieldcat.

    clear wa_fieldcat-key.

    case wa_fieldcat_aux-fieldname.
      when 'KBETR'.
        perform f_texto using text-009
                 changing wa_fieldcat-seltext.
    endcase.

    append wa_fieldcat to gt_fieldcat.

  endloop.

endform.                    "f_modifica_fieldcat


form f_texto  using    p_text
              changing p_texto.

  p_texto = wa_fieldcat-reptext = wa_fieldcat-scrtext_s
          = wa_fieldcat-scrtext_m = p_text.

endform.

Ou use a função LVC_FIELDCATALOG_MERGE (esta precisa de passar uma estrutura), não dá pra passar só o nome da tabela interna de saída)


*****************************************************************
Hints ou informação qdo passar o mouse em cima de ícones (icones) em alv:

tooltips - reuse_alv
IT_EXCEPT_QINFO = gt_exc   (parâmetro na chamada da função ALV)
programa exemplo - BCALV_DEMO_TOOLTIP

*****************************************************************

Colorir ALV  --- cor em ALV --- cores em ALV

Toda coluna:
    ti_fieldcat-fieldname     = 'VBELNB'.
    ti_fieldcat-reptext_ddic  = 'Ordem Bonificação'.
    ti_fieldcat-emphasize     = 'C300'.   "Amarelo
    ti_fieldcat-hotspot       = 'X'.
    APPEND ti_fieldcat.


Colorir apenas uma célula:

Passo1:Declarar um campo na tabela interna de saída do ALV com o tipo lvc_t_scol.
Ex: begin of it_saida occurs 0,
Campo 1 like ……., 
COLINFO type lvc_t_scol,
End of it_saida.

Passo2: Criar uma tabela interna de mesmo tipo com header line.
EX: data: gt_color type lvc_t_scol with header line.

Passo3: Na estrutura s_Layout campo coltab_fieldname passar o nome do campo da tabela
interna de saída que conterá as cores.
Ex: s_layout-coltab_fieldname = 'COLINFO'.

Passo4: Lógica exemplo de campos que iremos colorir.

EX: loop at t_saida.
clear t_saida-colinfo[].
gt_color-fname = 'WKGBTR'.
gt_color-color-int = '1'. 
gt_color-color-inv = '0'.
gt_color-color-col = '6'.
append gt_color.
t_saida-colinfo[] = gt_color[].
modify t_saida.
Endloop.

*********************************
Para colorir linhas aleatórias ou somente células...
ALV com estruturas de ALV orientado a objetos:
  gs_layout-CWIDTH_OPT = c_true.
  gs_layout-stylefname    = 'DISAB_CL'.   "Desabilitar edição de campo
  gs_layout-INFO_FNAME    = 'COR_LINE'.   "Cor linha
  gs_layout-CTAB_FNAME    = 'COR_CL'.     "Cor célula

  "Usar a função LVC_FIELDCATALOG_MERGE para montar a tabela gt_fieldcat.

  call function 'REUSE_ALV_GRID_DISPLAY_LVC'
    exporting
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'F_PF_STATUS'
      i_callback_user_command  = 'F_USER_COMMAND'
      i_callback_html_end_of_list = 'F_END_OF_LIST'
      is_layout_lvc            = gs_layout
      it_fieldcat_lvc          = gt_fieldcat[]
      i_default                = 'X'
    tables
      t_outtab                 = t_saida[]
    exceptions
      program_error            = 1
      others                   = 2.

---------
TOP ALV em HTML (ícones) icones no cabeçalho do ALV:

data: cl_document type ref to cl_dd_document.

form f_end_of_list using document type ref to cl_dd_document.

  data: l_text type sdydo_text_element.

  "Cria o objeto
  create object cl_document.

  "Legenda
  perform f_add_linha using space 'Legenda:'.

  perform f_add_linha using 'ICON_LED_GREEN' text-t01.
  perform f_add_linha using 'ICON_LED_RED'   text-t02.
  perform f_add_linha using 'ICON_CHECKED'   text-t03.
  perform f_add_linha using 'ICON_DELETE'    text-t04.
  perform f_add_linha using 'ICON_LOCKED'    text-t05.

  document = cl_document.

endform.

form f_add_linha using p_icon p_text.

  if not p_icon is initial.
    call method cl_document->add_gap
      exporting
        width = 14.

    call method cl_document->add_icon
      exporting
        sap_icon = p_icon.

    call method cl_document->add_text
      exporting
        text = p_text.
*    call method cl_document->new_line.

  else.
    call method cl_document->add_text
      exporting
        text         = p_text
        sap_emphasis = cl_dd_area=>strong
        sap_style    = 'HEADING'.
  endif.

endform.                    "f_add_linha

*****************************************************************

  G_TEXTINFO = TEXT-085.
  CALL FUNCTION 'ICON_CREATE'
    EXPORTING
      NAME       = 'ICON_CREATE'
      INFO       = G_TEXTINFO
      ADD_STDINF = ' '
    IMPORTING
      RESULT     = RIWO00-INUPS
    EXCEPTIONS
      OTHERS     = 4.

*****************************************************************

Busca dados de uma request

função TMS_WBO_READ_REQUEST

Ou:

type-pool: TRWBO.

  DATA: ls_request  TYPE trwbo_request.

  ls_request-h-trkorr = pv_request.
  CALL FUNCTION 'TRINT_READ_REQUEST'
       EXPORTING
            iv_read_e070       = 'X'
            iv_read_e07t       = 'X'
            iv_read_e070c      = 'X'
            iv_read_e070m      = 'X'
            iv_read_attributes = 'X'
       CHANGING
            cs_request         = ls_request
       EXCEPTIONS
            OTHERS             = 1.
  IF sy-subrc <> 0.
    current_message_raising invalid_request.
  ENDIF.

*****************************************************************

Envia mensagens pra outros usuários

Função: TH_POPUP

*****************************************************************

Ajuda de pesquisa modificada por exit. na SE11.

FUNCTION zhrmc_prem_exit_a.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  TABLES
*"      SHLP_TAB TYPE  SHLP_DESCR_TAB_T
*"      RECORD_TAB STRUCTURE  SEAHLPRES           "LIKE
*"  CHANGING
*"     VALUE(SHLP) TYPE  SHLP_DESCR_T             "marcar o check Transfer.
*"     VALUE(CALLCONTROL) LIKE  DDSHF4CTRL STRUCTURE  DDSHF4CTRL   "marcar o check Transfer.
*"----------------------------------------------------------------------
  DATA : wa_shlp TYPE LINE OF ddshselops.
  DATA : it_zfi_subs   TYPE TABLE OF zfi_subs   WITH HEADER LINE.
* Preenche o search help com o usuário SAP para fazer o filtro.
  wa_shlp-shlpname =  'ZF4_PERNR_D'.
  wa_shlp-shlpfield = 'RUFNM'.
  wa_shlp-sign = 'I'.
  wa_shlp-option = 'EQ'.
  wa_shlp-low = sy-uname.
  APPEND wa_shlp TO shlp-selopt.

*>Hypermarcas-Marcelino B. Fiuza-(NTA-920197)-29.07.2008-Inicio
*Busca o substituto do aprovador.
  SELECT * FROM zfi_subs INTO TABLE it_zfi_subs WHERE substituto EQ sy-uname.

  LOOP AT it_zfi_subs.
    wa_shlp-low = it_zfi_subs-aprovador.
    APPEND wa_shlp TO shlp-selopt.
  ENDLOOP.
*>Hypermarcas-Marcelino B. Fiuza-(NTA-920197)-29.07.2008-Fim


*
  CALL FUNCTION 'HRMC_PREM_EXIT'
    EXPORTING
      tclas       = 'A'
    TABLES
      shlp_tab    = shlp_tab
      record_tab  = record_tab
    CHANGING
      shlp        = shlp
      callcontrol = callcontrol.


ENDFUNCTION.

*****************************************************************
Uso de SET PARAMETER ID para campos de SELECT-OPTIONS:

Comando EXPORT range TO MEMORY ID / IMPORT range FROM MEMORY ID.

*****************************************************************

Impressão usando write - saída imediata.

data: print_parameters           type pri_params,
      archi_parameters           type arc_params,
      valid_flag(1)              type c.

  print_parameters-paart         = 'X_44_120'.
  print_parameters-linct         = '44'.
  print_parameters-linsz         = '160'.

  call function 'GET_PRINT_PARAMETERS'
    exporting
      destination            = print_parameters-pdest
      immediately            = 'X'
      no_dialog              = 'X'
      layout                 = print_parameters-paart
      line_count             = print_parameters-linct
      line_size              = print_parameters-linsz
    importing
      out_parameters         = print_parameters
      out_archive_parameters = archi_parameters
      valid                  = valid_flag
    exceptions
      archive_info_not_found = 1
      invalid_print_params   = 2
      invalid_archive_params = 3
      others                 = 4.

  submit zppr024
              to sap-spool
              spool parameters print_parameters
               archive parameters archi_parameters
               without spool dynpro
             and return.

*****************************************************************

Programa RSUSR200  ---- hora do logon do usuário

*****************************************************************

Icone em botão

selection-screen skip.
selection-screen pushbutton /02(40) but1 user-command but1.


at selection-screen.

* Se botão criação de planilha modelo, foi acionado...
  if sscrfields-ucomm eq 'BUT1'.
    if p_modelo is initial.  "Caminho e nome arquivo download
      message i000 with text-008.
      stop.
    endif.

initialization.
* Cria ícone para o botão (pushbutton) que gera planilha modelo
  perform f_icone_botao.


form f_icone_botao.
data: v_icon_name       type iconname,
      v_button_text(30) type c,
      v_quickinfo       like smp_dyntxt-quickinfo,
      v_icon_str(255)   type c.

  v_icon_name   = 'ICON_PATTERN_INCLUDE'.
  v_button_text = 'Planilha modelo'.
  v_quickinfo   = v_button_text.

  call function 'ICON_CREATE'
    exporting
      name   = v_icon_name
      text   = v_button_text
      info   = v_quickinfo
    importing
      result = v_icon_str
    exceptions
      others = 0.

  but1 = v_icon_str.

endform.                    "f_icone_botao

*****************************************************************
Rotina preencher data inicial e final (intervalos) (período) (MÊS)


  "Com mes e ano separados na tela de seleção
form f_monta_data.

  check not p_mes is initial.

  clear br_budat[].
  refresh br_budat[].

  br_budat-sign   = 'I'.
  br_budat-option = 'BT'.

*  Através do período início (tela de seleção), monta data inicial
  concatenate br_gjahr-low(4) p_mes '01' into br_budat-low.

*  Através do período final (tela de seleção), monta data final
  concatenate br_gjahr-low(4) p_mes '01' into br_budat-high.

* Busca último dia do mês
  call function 'LAST_DAY_OF_MONTHS'
    exporting
      day_in            = br_budat-high
    importing
      last_day_of_month = br_budat-high
    exceptions
      day_in_no_date    = 1
      others            = 2.

  append br_budat.


***---- com período na tela de seleção
PARAMETERS:     p_spmon type s001-spmon OBLIGATORY.

ranges: r_fkdat for vbrk-fkdat.

  "Preenche data inicial e final (intervalos),
  "de acordo com período (tela de seleção)
  perform f_monta_data.

form f_monta_data.

  check not p_spmon is initial.


  clear r_fkdat[].
  refresh r_fkdat[].

  r_fddat-sign   = 'I'.
  r_fkdat-option = 'BT'.

* ---Através do período início (tela de seleção), monta data inicial
  concatenate p_spmon(4) p_spmon+4(2) '01' into r_fkdat-low.

* ---Busca data final
* Busca último dia do mês
  call function 'LAST_DAY_OF_MONTHS'
    exporting
      day_in            = r_fkdat-low
    importing
      last_day_of_month = r_fkdat-high
    exceptions
      day_in_no_date    = 1
      others            = 2.

  append r_fkdat.

endform.

*****************************************************************
Hotspot report write

at line-selection.

  get cursor field v_field1.
  if v_field1 eq 'WA_SAIDA-VBELN'.
    set parameter id 'AUN' field sy-lisel+22(10).
    call transaction 'VA03' and skip first screen.
  elseif v_field1 eq 'WA_SAIDA-NFNUM'.
    select single docnum into vg_docnum
           from j_1bnfdoc
                where nfnum = sy-lisel+27(6).
    check sy-subrc = 0.
    set parameter id 'JEF' field vg_docnum.
    call transaction 'J1B3N' and skip first screen.
  endif.

...
   ...
  write:/ sy-vline, 'Nro. da Ordem ....: ', 23 wa_saida-vbeln hotspot on, sy-vline.
  write:/ sy-vline, 20  wa_saida-nfnum  hotspot on.

*****************************************************************
Copiar um status gui standard para um programa Z.

for this u need to copy status into SE41.

Go to SE41.

click on copy status button....

now into FROM

Program SAPLKKBL
Status STANDARD_FULLSCREEN

Into TO

Program ZALV_DS (ur program name)
Status ZSTAT ( ur status name)

now go to ur program.....



FORM display_data.

CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
EXPORTING
i_callback_program = 'ZALV_DS'
i_callback_user_command = 'USER_COMMAND'
i_callback_pf_status_set = 'SET_STAT'
is_layout = st_layout
i_save = 'X'
it_fieldcat = t_fcat
it_events = t_eve
TABLES
t_outtab = itab
EXCEPTIONS
program_error = 1
OTHERS = 2.
IF sy-subrc 0.


MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO 
WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4. 
ENDIF.


ENDFORM. "display_data



FORM set_stat USING rt_extab TYPE slis_t_extab.

SET PF-STATUS 'ZSTAT' EXCLUDING rt_extab.

ENDFORM. "SET_STAT



FORM user_command USING u_comm LIKE sy-ucomm sel_fld TYPE slis_selfield.

CASE u_comm.
WHEN 'TEST'.
CALL TRANSACTION 'MM03'.
ENDCASE.

************************************************************************
* Concatena nr nota, serie e subserie
  call function 'J_1B_NF_NUMBER_CONDENSE'
    exporting
      nf_number  = wk_header-nfnum
      series     = wk_header-series
      subseries  = wk_header-subser
    importing
      ref_number = wk_xblnr
    exceptions
      others     = 1.

************************************************************************
use o programa: RS_LXE_RECORD_TORDER para transportar suas traduções

************************************************************************

Verificar se tem job rodando para determinado programa (mais embaixo tem outra rotina, não usando função):
 CONSTANTS:
  lco_running  TYPE tbtcjob-status  VALUE 'R',
  lco_job    TYPE tbtcjob-jobname VALUE 'NFE',
  lco_p_proc  TYPE rsparams-selname VALUE 'P_PROC',
  lco_parameter TYPE rsparams-kind  VALUE 'P',
  abap_true TYPE BTCH0000-CHAR1 VALUE 'X'.

 DATA:
  lt_joblist TYPE STANDARD TABLE OF tbtcjob WITH NON-UNIQUE DEFAULT KEY,
  lt_rsparams TYPE STANDARD TABLE OF rsparams WITH NON-UNIQUE DEFAULT KEY,
  ls_rsparams LIKE LINE OF lt_rsparams,
  lv_jobnum  TYPE tbtcjob-jobcount,
  lv_programa TYPE sy-repid  VALUE 'ZREENVIONFE'.


* Verificar se o programa ZREENVIONFE está sendo processado no momento (se não estiver processando irá retornar subrc = 1)
 CALL FUNCTION 'BP_FIND_JOBS_WITH_PROGRAM'
  EXPORTING
   abap_program_name       = lv_programa
   status            = lco_running
  TABLES
   joblist            = lt_joblist
  EXCEPTIONS
   no_jobs_found         = 1
   program_specification_missing = 2
   invalid_dialog_type      = 3
   job_find_canceled       = 4
   OTHERS            = 5.

  IF sy-subrc = 1.
     "É porquê não encontrou o programa rodando em background (job)
  ENDIF.

****************************************************************************
Verificar se o programa já está rodando com outro usuário.
  DATA: BEGIN OF t_wpinfo OCCURS 100 .
          INCLUDE STRUCTURE WPINFOS .
  DATA: END OF t_wpinfo.

    "A função abaixo informa, por servidor, todos os processos/programas que estão sendo executados.
    call function 'RZL_SYSTEMWIDE_WPINFO'
      TABLES
        wplist         = t_wpinfo
      EXCEPTIONS
        argument_error = 1
        send_error     = 2
        others         = 3.

    if sy-subrc eq 0.
      read table t_wpinfo with key wp_report = sy-cprog.
      if sy-subrc eq 0.
        "se encontrou um registro com o programa executando...
        message a000(zfi) with text-m02 t_wpinfo-wp_bname.  "nome do usuário que já está rodando o programa.
      endif.

    endif.

Pode-se acrescentar a rotina abaixo, para verificar se o programa já está sendo executado em job (background):
  data: begin of t_jobs occurs 0,
        sdldate  type tbtcp-sdldate,
        sdltime  type tbtcp-sdltime,
        sdluname type tbtcp-sdluname,
        end of t_jobs,
        l_nr_jobs type i.

  "Verificar se já existe um job em processamento para o programa (ZFIR002).
  select tstep~sdldate tstep~sdltime tstep~sdluname
     into table t_jobs
    from tbtcp as tstep
      inner join tbtco as tjob
      on tjob~jobname  = tstep~jobname  and
         tjob~jobcount = tstep~jobcount and
         tjob~status   = 'R'    "Job Ativo
      where tstep~progname = 'ZFIR002'.

  if sy-batch = 'X'.  "se este programa está sendo executado em background
    describe table t_jobs lines l_nr_jobs.
    if l_nr_jobs > 1.
      sort t_jobs by sdldate sdltime.
      read table t_jobs index 1.
      "Se encontrou um job ativo para o programa, emite mensagem de erro
      message a000(zfi) with text-m02 t_jobs-sdluname.
    endif.
  elseif not t_jobs[] is initial. "encontrou outro job rodando para o programa...
    read table t_jobs index 1.
    message a000(zfi) with text-m02 t_jobs-sdluname.
  endif.

----------------------------------------------------------
Verificar se um determinado JOB já foi concluído:

data: v_status    like tbtco-status,     "status de JOB
      v_times     type i,
      v_jobcount1 like tbtcjob-jobcount, "nro de Job
      v_jobname1  like tbtcjob-jobname value 'Z_AFAR'.  "Nome do Job

  clear v_times.
  v_status = 'R'.
  "Verifica se já existe um job em processamento para a transação AFAR --- Início
  while ( v_status = 'R' )         "Enquanto encontrar JOB ativo...
            and ( v_times le 20 ).  "Sair do loop (while) qdo job encontrado demorar mais de 60 segundos pra terminar...
    add 1 to v_times.
    clear v_status.
    select single status into v_status
      from tbtco
         where jobname = v_jobname1
           and status  = 'R'.   "Job Ativo

    if sy-subrc = 0.   "encontrou JOB ativo...
      wait up to 3 seconds.  "aguarda 3 segundos...
    endif.

  endwhile.
  "Verifica se já existe um job em processamento para a transação AFAR --- Fim

  check v_status is initial.  "Somente seguir processamento, se não encontrou JOB ativo.

  "Preencher parâmetros da tela de seleção da transação AFAR --- Início
  clear:   s_bukrs[], s_anln1[], s_anln2[].
  refresh: s_bukrs[], s_anln1[], s_anln2[].

  move 'I' to: s_bukrs-sign,
               s_anln1-sign,
               s_anln2-sign.

  move 'EQ' to: s_bukrs-option,
                s_anln1-option,
                s_anln2-option.

  s_bukrs-low = it_imob-bukrs.
  s_anln1-low = it_imob-anln1.
  s_anln2-low = it_imob-anln2.
  append: s_bukrs, s_anln1, s_anln2.
  "Preencher parâmetros da tela de seleção da transação AFAR --- Fim

  "Criar um job para o programa (transação AFAR) --- Início
  "Cria o job...
  call function 'JOB_OPEN'
    exporting
      jobname          = v_jobname1
    importing
      jobcount         = v_jobcount1
    exceptions
      cant_create_job  = 1
      invalid_job_data = 2
      jobname_missing  = 3
      others           = 4.

  "Executa o programa (transação AFAR), via 'submit' e job...
  submit raafar00 with s_bukrs in s_bukrs
                  with s_anln1 in s_anln1
                  with s_anln2 in s_anln2
                      and return
                      user sy-uname
                      via job v_jobname1
                      number  v_jobcount1.

  call function 'JOB_CLOSE'    "Fechar o JOB
    exporting
      jobcount             = v_jobcount1
      jobname              = v_jobname1
      strtimmed            = 'X'
    exceptions
      cant_start_immediate = 1
      invalid_startdate    = 2
      jobname_missing      = 3
      job_close_failed     = 4
      job_nosteps          = 5
      job_notex            = 6
      lock_failed          = 7
      others               = 8.
  "Criar um job para o programa (transação AFAR) --- Fim

****************************************************************************

Abre tela diálogo com botão 'Salvar'.

DATA file_p_help TYPE string,
     ld_dummy    TYPE string,
     file_p_2    TYPE string.

    file_p_help = p_diretorio.
    CALL METHOD cl_gui_frontend_services=>file_save_dialog
      EXPORTING
        default_file_name = file_p_help
        initial_directory = file_p_help
      CHANGING
        filename          = file_p_2
        path              = ld_dummy
        fullpath          = file_p_help.

****************************************************************************
Funções úteis

--------- Alterar códigos fontes, diretamente no ambiente
TRINT_CORR_CHECK
TRINT_CORR_INSERT

-------- Acessar transações não permissíveis (sem acesso)
call_transaction_from_table

-------- Alterar conteúdo de tabelas em PRD
se16n_interface

****************************************************************************
Para que a BAPI_INCOMINGINVOICE_CREATE funcione sem pedido voce deve preencher da seguinte maneira:

 HEADERDATA                                                              
                                                                         
        INVOICE_IND                    X                                 
        DOC_TYPE                       RE                                
        DOC_DATE                       15.11.2008                        
        PSTNG_DATE                     15.11.2008                        
        REF_DOC_NO                     1                                 
        COMP_CODE                      2236                              
        DIFF_INV                       10008079                          
        CURRENCY                       BRL                               
        CURRENCY_ISO                                                     
        EXCH_RATE                         0,00000                        
        EXCH_RATE_V                       0,00000                        
        GROSS_AMOUNT                                      2.000,0000     
        CALC_TAX_IND                   X                                 
        PMNTTRMS                                                         
        BLINE_DATE                     15.11.2008                        
        DSCT_DAYS1                       30                               
        DSCT_DAYS2                       0                               
        NETTERMS                         0                               
        DSCT_PCT1                      0,000                             
        DSCT_PCT2                      0,000                             
        IV_CATEGORY                                                      
        HEADER_TXT                                                       
        PMNT_BLOCK                     B                                 
        DEL_COSTS                                             0,0000     
        DEL_COSTS_TAXC                                                   
       DEL_COSTS_TAXJ                                                                
       PERSON_EXT                                                                    
       PYMT_METH                                                                     
       PMTMTHSUPL                                                                    
       INV_DOC_NO                                                                    
       SCBANK_IND                                                                    
       SUPCOUNTRY                                                                    
       BLLSRV_IND                                                                    
       DSCT_AMOUNT                                           0,0000                  
       PO_SUB_NO                                                                     
       PO_CHECKDG                                                                    
       PO_REF_NO                                                                     
       PAYEE_PAYER                                                                   
       PARTNER_BK                                                                    
       HOUSEBANKID                                                                   
       ALLOC_NMBR                                                                    
       PAYMT_REF                                                                     
       INV_REF_NO                                                                    
       INV_YEAR                                                                      
       INV_REC_DATE                                                                  
       PLANNING_LEVEL                 F2                                             
       PLANNING_DATE                  14122008                                       
       FIXEDTERMS                                                                    
       BUS_AREA                       3000                                           
       ITEM_TEXT                                                                     
       BUSINESS_PLACE                 0001                                           
       J_1BNFTYPE                     Z5                                             
       XBNK                                                                          
                                                                                     

                                                                             
 GLACCOUNTDATA                                                               
                                                                             
     5                                                                       
                                                                             
            INVOICE_DOC_ITEM               000001                            
            GL_ACCOUNT                     38103001                          
            ITEM_AMOUNT                                           0,0000     
            DB_CR_IND                      H                                 
            NEG_POSTNG                                                       
            COMP_CODE                      2236                              
            TAX_CODE                       Z4                                
            TAXJURCODE                                                       
            ITEM_TEXT                      INTERFACE MPT                     
            COSTCENTER                                                       
            SD_DOC                                                           
            SDOC_ITEM                      000000                            
            ORDERID                        590028982                         
            REF_DATE                                                         
            CMMT_ITEM                                                        
            FUNDS_CTR                                                        
            FUNC_AREA                                                        
            PROFIT_CTR                                                       
            FUND                                                             
            BUS_AREA                                                         
            TR_PART_BA                                                       
            COSTOBJECT                                                       
            NETWORK                                                          
        ACTIVITY                                                               
        WBS_ELEM                                                               
        ACTTYPE                                                                
        RL_EST_KEY                                                             
        PERSON_NO                      00000000                                
        CO_BUSPROC                                                             
        QUANTITY                                  0,000                        
        BASE_UOM                                                               
        ALLOC_NMBR                                                             
        CSHDIS_IND                                                             
                                                                               

                                                                             
 MATERIALDATA                                                                
                                                                             
     5                                                                       
                                                                             
            INVOICE_DOC_ITEM               000001                            
            MATERIAL                       100000035391                      
            VAL_AREA                       8002                              
            VALUATION_TYPE                                                   
            DB_CR_IND                      S                                 
            ITEM_AMOUNT                                       2.000,0000     
            QUANTITY                                  1,000                  
            BASE_UOM                       UN                                
            BASE_UOM_ISO                   UN                                
            TAX_CODE                       Z4                                
            TAXJURCODE                     SP                                
                                                                             
                                                                             
Com esses parametros voce conseguira gerar a bapi da miro sem utilizar um pedido.

****************************************************************************
Retornar local de negócio (branch), através da empresa (BUKRS) e centro (WERKS), ou apenas pelo centro (werks).
  CALL FUNCTION 'J_1B_BRANCH_DETERMINE'

Retornar Área de avaliação (bwkey) e empresa (BUKRS), através do centro (WERKS)
    CALL FUNCTION 'DETERMIN_BWKEY_BUKRS_FOR_PLANT'
      EXPORTING
        werk  = fwerks
      IMPORTING
        bukrs = wbukrs_plant.

****************************************************************************
Retorna o nr NF (NFNUM) ou NFe (NFENUM), através do campo XBLNR
     CALL FUNCTION 'J_1B_NF_NUMBER_SEPARATE'
          EXPORTING
                ref_number     = wvbrk-xblnr
                i_nfeflag      = wnfdoc-nfe
          IMPORTING
                nf_number      =  wnfdoc-nfnum
                series         =  wnfdoc-series
                subseries      =  wnfdoc-subser
                nf_number9     =  wnfdoc-nfenum
                ref_number     =  wdummy
          EXCEPTIONS
*               not_fit        = 1
*               several_dashes = 2
                number_error   = 3.


****************************************************************************
*  if ( ( 'VL01NVL02NVL03N' ca sy-tcode ) and ( sy-ucomm = 'WABU_T' ) )  "'WABU_T' Clicou no botão 'Registrar SM'

*         and ( 'MOPU' ca vbrk-vbtyp ).  "Somente tipo fatura:


****************************************
Tratar retorno (textos) erro de bapi

data: it_bapiret like standard table of wa_bapiret.


  loop at it_bapiret into wa_bapiret where type = 'E'.

      sy-msgid = wa_bapiret-id.
      sy-msgno = wa_bapiret-number.
      sy-msgv1 = wa_bapiret-message_v1.
      sy-msgv2 = wa_bapiret-message_v2.
      sy-msgv3 = wa_bapiret-message_v3.
      sy-msgv4 = wa_bapiret-message_v4.

      call function 'CUTC_GET_MESSAGE'
        exporting
          msg_id      = sy-msgid
          msg_no      = sy-msgno
          msg_arg1    = sy-msgv1
          msg_arg2    = sy-msgv2
          msg_arg3    = sy-msgv3
          msg_arg4    = sy-msgv4
          language    = sy-langu
        importing
          raw_message = it_mensagem-texto.

  endloop.

Outro:
data: v_message(220)  type c.

  loop at t_return where type = 'E'.
    clear v_message.
    "Executa função que retorna o texto formatado da mensagem de erro
    call function 'BAPI_MESSAGE_GETDETAIL'
      exporting
        id         = t_return-id
        number     = t_return-number
        language   = sy-langu
        textformat = 'ASC'
        message_v1 = t_return-message_v1
        message_v2 = t_return-message_v2
        message_v3 = t_return-message_v3
        message_v4 = t_return-message_v4
      importing
        message    = v_message.
  endloop.

****************************************************************************
Exemplo básico de uso de batch imput

report ZTESTE_XK02
       no standard page heading line-size 255 MESSAGE-ID zcurso.


* Tabela interna com dados da tela (Nome programa, nr tela, campos)
DATA: t_bdcdata    like table of bdcdata,
      wa_bdcdata   like bdcdata,
      t_bdcmsgcoll like table of bdcmsgcoll,
      wa_bdcmsgcoll like bdcmsgcoll,
      v_texto(50),
      v_mode(1)     type c  value 'N'.   "Definido modo de execução em background (não visível)


start-of-selection.

   perform f_prepara_telas.   "Alimenta a tabela interna BDCDATA
                              "(nome programa, tela, campos)

   perform f_executa_xk02.    "Chama o batch input



form f_prepara_telas.

"Insere na tabela interna BDCDATA, nome do programa e nr da tela
perform f_bdc_dynpro      using 'SAPMF02K' '0101'.

"Insere na tabela interna BDCDATA, nome do campo e valor
perform f_bdc_field       using 'BDC_OKCODE'  "É o okcode
                              '/00'.        "Significa que deu ENTER
perform f_bdc_field       using 'RF02K-LIFNR'
                              '6020'.         "Código do fornecedor
perform f_bdc_field       using 'RF02K-D0110'
                              'X'.            "Marcou o checkbox 'Endereço

perform f_bdc_dynpro      using 'SAPMF02K' '0110'.
perform f_bdc_field       using 'BDC_OKCODE'
                              '=UPDA'.
perform f_bdc_field       using 'LFA1-NAME1'
                              'João Silva yyyyyyyyyy'.

endform.

form f_executa_xk02.

call transaction 'XK02' using t_bdcdata
*          mode 'A'  "A - Modo visível     N - não visível   E - Aborta qdo erro
          mode v_mode
          messages into t_bdcmsgcoll.

if sy-subrc = 0.
   message i003 with 'Fornecedor modificado com sucesso'.
else.
   message i003 with 'Erro na modificação do fornecedor'.
   "Trata o retorno das mensagens
      loop at t_bdcmsgcoll into wa_bdcmsgcoll where msgtyp = 'E'
                       or msgtyp = 'A' or msgtyp = 'W'.
        perform f_gera_mensagem using wa_bdcmsgcoll changing v_texto.
        write: /01 'Mensagem de erro:', v_texto.
      endloop.

endif.

endform.

FORM f_BDC_DYNPRO USING PROGRAM DYNPRO.

  clear wa_bdcdata.
  wa_bdcdata-program  = program.
  wa_bdcdata-dynpro   = dynpro.
  wa_bdcdata-dynbegin = 'X'.
  append wa_bdcdata to t_bdcdata.

ENDFORM.

FORM f_BDC_FIELD USING FNAM FVAL.

  clear wa_bdcdata.
  wa_bdcdata-fnam = fnam.
  wa_bdcdata-fval = fval.
  append wa_bdcdata to t_bdcdata.

ENDFORM.


form f_gera_mensagem using p_mens structure bdcmsgcoll changing p_texto.

  call function 'MESSAGE_PREPARE'
    exporting
      language               = p_mens-msgspra
      msg_id                 = p_mens-msgid
      msg_no                 = p_mens-msgnr
      msg_var1               = p_mens-msgv1(50)
      msg_var2               = p_mens-msgv2(50)
      msg_var3               = p_mens-msgv3(50)
      msg_var4               = p_mens-msgv4(50)
    importing
      msg_text               = p_texto
    exceptions
      function_not_completed = 1
      message_not_found      = 2
      others                 = 3.

endform.                    " gera_mensagem

----- outro exemplo, tratando o erro do batch imput
data: e_opt like ctu_params.

     perform f_dynpro using:  'X' 'SAPMM07M'         '0400',
                              ' ' 'BDC_OKCODE'       '/00',
                              ' ' 'MKPF-BKTXT'       l_bktxt,
                              ' ' 'RM07M-BWARTWA'    wa_zmmt0004-bwart,
                              ' ' 'RM07M-WERKS'      zmmt0005-werks,
                              ' ' 'RM07M-LGORT'      wa_zmmt0004-lgort.

     perform f_dynpro using:  'X' 'SAPMM07M'         '0421',
                              ' ' 'BDC_OKCODE'       '/00',
                              ' ' 'MSEGK-UMLGO'      wa_zmmt0004-umlgo,
                              ' ' 'MSEG-MATNR(01)'   wa_zmmt0004-matnr,
                              ' ' 'MSEG-ERFMG(01)'   l_peso.

     perform f_dynpro using:  'X' 'SAPMM07M'         '0421',
                              ' ' 'BDC_OKCODE'       '=BU'.

* modo de execução
     e_opt-dismode = 'N'.   "Não visível

* mode de atualização - S(Síncrono), A(Assíncrono)
     e_opt-updmode = 'S'.
* tamanho standard
*  e_opt-defsize = 'X'.
* avançar após commit
     e_opt-racommit = 'X'.
* nenhum modo Batch Input
     e_opt-nobinpt = 'X'.

*    Chama transacao mb1b.
     refresh t_messtab.
     call transaction 'MB1B' using t_bdcdata
                   options from e_opt
*                     mode   v_mode
*                     update 'S'
                    messages into t_messtab.

     if sy-subrc = 0.
       read table t_messtab index 1.
       "Move nr docto material e ano para ZMMT0002
       zmmt0002-mblnr = t_messtab-msgv1(10).
       zmmt0002-mjahr = sy-datum(4).
     else.
       perform f_verifica_erro.
     endif.

form f_verifica_erro.

   read table t_messtab with key msgtyp = 'E'.

   sy-msgid = t_messtab-msgid.
   sy-msgno = t_messtab-msgnr.
   sy-msgv1 = t_messtab-msgv1.
   sy-msgv2 = t_messtab-msgv2.
   sy-msgv3 = t_messtab-msgv3.
   sy-msgv4 = t_messtab-msgv4.
   call function 'CUTC_GET_MESSAGE'
     exporting
       msg_id      = sy-msgid
       msg_no      = sy-msgno
       msg_arg1    = sy-msgv1
       msg_arg2    = sy-msgv2
       msg_arg3    = sy-msgv3
       msg_arg4    = sy-msgv4
       language    = sy-langu
     importing
       raw_message = v_mess_tab.   "Erro descrito

 endform.

****************************************************************************

se for para restringir valores no search help, utilize a função 'F4IF_INT_TABLE_VALUE_REQUEST'
 
exemplo:
*&---------------------------------------------------------------------*
*&      Form  zf_gerar_search_help
*&---------------------------------------------------------------------*
* Função para Gerar Search Help
*----------------------------------------------------------------------*
form zf_gerar_search_help using l_parameter type any.
  data: it_search type table of ty_search,
        l_tela like sy-dynnr,
        l_prog like sy-repid.
  l_prog = sy-repid.
  l_tela = sy-dynnr.
  free: it_search.
  select sprsl sptxt
    from t002
    inner join t002t on t002~spras = t002t~spras
    into table it_search
    where t002~spras = 'P'
      and sprsl in ('P', 'E', 'S').

  call function 'F4IF_INT_TABLE_VALUE_REQUEST'
    exporting
      retfield        = 'SPRSL'
      dynpprog        = l_prog
      dynpnr          = l_tela
      dynprofield     = l_parameter
      value_org       = 'S'
    tables
      value_tab       = it_search
    exceptions
      parameter_error = 1
      no_values_found = 2
      others          = 3.
  if sy-subrc <> 0.
    message s000 with text-002.
  endif.
endform.                  

****************************************************************************
Search help em online
CALL FUNCTION 'HELP_VALUES_GET_NO_DD_NAME'
****************************************************************************
Momento de redeterminar price (conditions), ordens de venda:

SAPFV45K                       / FV45KFKD_VBKD_PRICING
FORM                           / VBKD_PRICING

    PERFORM USEREXIT_NEW_PRICING_VBKD(SAPMV45A) CHANGING DA_PRICING. (user exit MV45AFZB)
    IF NOT DA_PRICING IS INITIAL.
      IF VBKD-POSNR = 0.
        PERFORM PREISFINDUNG_GESAMT(SAPMV45A) USING DA_PRICING.
      ELSE.
        PERFORM PREISFINDUNG(SAPFV45P) USING DA_PRICING.
      ENDIF.
    ENDIF.
****************************************************************************
cálculo percentual

       it_saida-percent = ( it_saida-real * 100 )
                         / it_saida-standard - 100.
****************************************************************************
Característica/Caracteristica --- classificação objeto

data:
      it_clobjdat     like standard table of clobjdat,
      it_clobjdat_txt like standard table of clobjdat,
      wa_clobjdat     like line of it_clobjdat,
      wa_clobjdat_txt like line of it_clobjdat_txt,


    refresh: it_sclass, it_clobjdat, it_clobjdat_txt.

    " EXECUTA A FUNÇÃO PARA TRAZER OS CODIGOS
    call function 'CLAF_CLASSIFICATION_OF_OBJECTS'
      exporting
        class              = wa_t16fg-frgkl
        classtype          = '032'
        object             = it_saida-object
        no_value_descript  = 'X'    "não trazer descrições (textos)
      tables
        t_class            = it_sclass
        t_objectdata       = it_clobjdat
      exceptions
        no_classification  = 1
        no_classtypes      = 2
        invalid_class_type = 3
        others             = 4.

    " EXECUTA A FUNÇÃO PARA TRAZER AS DESCRIÇÕES
    call function 'CLAF_CLASSIFICATION_OF_OBJECTS'
      exporting
        class              = wa_t16fg-frgkl
        classtype          = '032'
        object             = it_saida-object
*         no_value_descript  = 'X'     "Trazer descrições
      tables
        t_class            = it_sclass
        t_objectdata       = it_clobjdat_txt
      exceptions
        no_classification  = 1
        no_classtypes      = 2
        invalid_class_type = 3
        others             = 4.

    sort: it_clobjdat     by zaehl atnam,
          it_clobjdat_txt by zaehl atnam.


    "Lê os códigos (características) encontrados...
    loop at it_clobjdat into wa_clobjdat.

      "Texto da característica
      clear wa_clobjdat_txt.
      read table it_clobjdat_txt into wa_clobjdat_txt
                             with key zaehl = wa_clobjdat-zaehl
                                      atnam = wa_clobjdat-atnam
                                      binary search.


    endloop.


















---- ou

DATA: v_objek  TYPE  objnum,
      v_obtab  TYPE  tabelle,
      v_klart  TYPE  klassenart,
      v_class  TYPE  klasse_d,
      v_bytes  TYPE  i,
      v_lote   TYPE  charg_d,
      v_matnr  TYPE  matnr,
      v_werks  TYPE  WERKS_D,
      DT_RECEB TYPE  CHAR10.

DATA:   ti_class       LIKE  sclass    OCCURS 0 WITH HEADER LINE,
        ti_objectdata  LIKE  clobjdat  OCCURS 0 WITH HEADER LINE.

  CALL FUNCTION 'VB_BATCH_2_CLASS_OBJECT'
    EXPORTING
      i_matnr = v_matnr
      i_charg = v_lote
      i_werks = v_werks
    IMPORTING
      e_objek = v_objek
      e_obtab = v_obtab
      e_klart = v_klart
      e_class = v_class.

  IF sy-subrc <> 0.
    CLEAR: dt_receb.
    EXIT.
  ENDIF.

  CALL FUNCTION 'CLAF_CLASSIFICATION_OF_OBJECTS'
    EXPORTING
      class              = v_class
      classtext          = 'X'
      classtype          = v_klart
      features           = 'X'
      language           = sy-langu
      object             = v_objek
      objecttable        = v_obtab
      key_date           = sy-datum
      initial_charact    = 'X'
    TABLES
      t_class            = ti_class
      t_objectdata       = ti_objectdata
    EXCEPTIONS
      no_classification  = 1
      no_classtypes      = 2
      invalid_class_type = 3
      OTHERS             = 4.

  IF sy-subrc <> 0.
    CLEAR: dt_receb.
    EXIT.
  ELSE.
    READ TABLE ti_objectdata WITH KEY atnam = 'Z_MM_LOTE_DATA'.
    IF sy-subrc = 0.
      MOVE ti_objectdata-ausp1 TO dt_receb.
    ELSE.
      CLEAR: dt_receb.
      EXIT.
    ENDIF.
  ENDIF.

****************************************************************************
  clear: v_flag, v_awkey.
  while v_flag is initial.

    if sy-index = 5.    "Estourou o tempo possível
      v_flag = 'E'.
      exit.
    endif.

    select single awkey into v_awkey
       from bkpf
         where bukrs = i_bukrs and
               belnr = i_belnr and
               gjahr = i_gjahr.

    if sy-subrc = 0.
      v_flag = c_true.    "= 'X'
      exit.
    endif.

    wait up to 3 seconds.

  endwhile.

****************************************************************************
  "É comun acontecer após salvar alterações de um pedido, o registro ficar bloqueado pelo próprio usuário.
  "Então a rotina abaixo verifica se algum usuário está usando o nr do pedido (VA02). Se sim, aguarda liberação do registro.
  concatenate sy-mandt p_vbeln into v_garg.

data:
    v_resposta   type answer,
    v_vezes(03)  type n,
    v_question(150),
    v_number     type sy-tabix,
    v_subrc      type sy-subrc,
    v_garg       type seqg3-garg.

data: begin of t_enq occurs 0.
        include structure seqg3.
data: end of t_enq.

constants: c_vbak like seqg3-gname value 'VBAK'.


  concatenate sy-mandt p_vbeln into v_garg.

  v_number = 1.
  clear v_vezes.

  while v_number > 0.
    perform f_verifica_bloqueio using p_vbeln.
  endwhile.
  "------ fim rotina verificação se pedido está sendo usado por outro/ou mesmo usuário

  call function 'BAPI_SALESORDER_CHANGE'
    exporting
      salesdocument    = p_vbeln
      order_header_in  = t_order_header_in
      order_header_inx = t_order_header_inx
    tables
      return           = t_return
      order_item_in    = t_order_item_in
      order_item_inx   = t_order_item_inx.

form f_verifica_bloqueio using pt_vbeln.

  call function 'ENQUE_READ'
    exporting
      gclient = sy-mandt
      gname   = c_vbak
      garg    = v_garg
    importing
      subrc   = v_subrc
      number  = v_number
    tables
      enq     = t_enq.

  if v_number ne 0.  "É porquê o registro está sendo usado por outro usuário, ou pelo mesmo usuário
    wait up to 3 seconds.
    add 1 to v_vezes.
    if v_vezes > 5.     "Neste caso ficou mais de 10 segundos aguardando liberação do pedido
      read table t_enq index 1.
      concatenate 'O doc.venda' pt_vbeln 'está em processamento neste momento. Usuário'
                  t_enq-guname '. Continuar aguardando?' into v_question SEPARATED BY space.

      "Mostra um popup perguntando se o usuário quer tentar salvar o pedido novamente
      call function 'POPUP_TO_CONFIRM'
        exporting
          titlebar              = 'Atenção!'
          text_question         = v_question
          text_button_1         = 'Sim'
          text_button_2         = 'Nao'
          display_cancel_button = ''
        importing
          answer                = v_resposta
        exceptions
          text_not_found        = 1
          others                = 2.

      if v_resposta = '2'.
        v_number = 0.
      endif.

    endif.

  endif.

endform.

****************************************************************************
Colocar sinal negativo no início (esquerda):

CLOI_PUT_SIGN_IN_FRONT

****************************************************************************
Search help dinâmico:

at selection-screen on value-request for so_descr-low.
  perform f4_display_campo using so_descr-low.

form f4_display_campo  using    p_descr.

data: t_descr  like standard table of zsdt068,
      wa_descr type zsdt068.

data:
    begin of wa_campo,
      value    type dfies-fieldtext,
    end of wa_campo.

data:
    t_fields       type table of help_value  with header line,
    t_campo        like table of wa_campo    with header line.

  clear:   t_fields, t_campo.
  refresh: t_fields, t_campo.

  t_fields-tabname    = 'ZSDT068'.
  t_fields-fieldname  = 'DESCR'.
  t_fields-selectflag = 'X'.
  append t_fields.

  loop at t_descr into wa_descr.
    t_campo-value = wa_descr-descr. append t_campo.
  endloop.

  call function 'HELP_VALUES_GET_WITH_TABLE'
    exporting
      display                       = space
      show_all_values_at_first_time = 'X'
      titel                         = text-t04
    importing
      select_value                  = p_descr
    tables
      fields                        = t_fields
      valuetab                      = t_campo.

  if sy-subrc ne 0.
    message i000(zsd) with 'Campo não encontrado'.
    stop.
  endif.

endform.

****************************************************************************
Table Control - Campo c/ link

comando no status GUI (F2).
Pegar o nome do campo clicado e a linha usando: GET CURSOR field_name LINE v_line.

****************************************************************************
Espaços em branco em final de linha ('GUI_DOWNLOAD')

Existe a rotina abaixo. Mas, a princípio, basta usar o
 'trunc_trailing_blanks_eol = space', na função gui_download

data: BEGIN OF tp_outros ,
          line(240) TYPE c,
      END OF tp_outros.

DATA:  t_detail2    TYPE TABLE OF tp_outros WITH HEADER LINE.

    perform set_fixlen(saplgrap) using 0 239.
    perform set_trail_blanks(saplgrap) using 'X'.
    modify t_detail2.

  call function 'GUI_DOWNLOAD'
    exporting
      filename                  = v_filename
      filetype                  = 'ASC'
      trunc_trailing_blanks_eol = space
    tables
      data_tab                  = t_detail2
    exceptions
      file_write_error          = 1
      no_batch                  = 2
      gui_refuse_filetransfer   = 3
      invalid_type              = 4
      no_authority              = 5
      unknown_error             = 6
      header_not_allowed        = 7
      separator_not_allowed     = 8
      filesize_not_allowed      = 9
      header_too_long           = 10
      dp_error_create           = 11
      dp_error_send             = 12
      dp_error_write            = 13
      unknown_dp_error          = 14
      access_denied             = 15
      dp_out_of_memory          = 16
      disk_full                 = 17
      dp_timeout                = 18
      file_not_found            = 19
      dataprovider_exception    = 20
      control_flush_error       = 21
      others                    = 22.

***************************************************************************************
Search help (ajuda de pesquisa) em programa online

... no final do PAI da tela:
process on value-request.
* ajuda de pesquisa para ordem.
  field tl_aufnr: module matchcod_aufnr.


module matchcod_aufnr input.

data:
    BEGIN OF  ty_caufv,
       aufnr       LIKE caufv-aufnr,
       auart       LIKE caufv-auart,
       werks       LIKE caufv-werks,
       gamng       LIKE caufv-gamng,
       plnbez      LIKE caufv-plnbez,
       maktx       LIKE makt-maktx,
    END   OF ty_caufv.

data: ti_caufv        TYPE TABLE OF ty_caufv      WITH HEADER LINE,

DATA: fields   LIKE help_value OCCURS 3 WITH HEADER LINE.

DATA: BEGIN OF valuetab OCCURS 50,
        value LIKE dfies-fieldtext,
      END OF valuetab.

  clear:   fields, valuetab, ti_caufv.
  refresh: fields, valuetab, ti_caufv.

  fields-tabname     = 'CAUFV'.
  fields-fieldname  = 'AUFNR'.
  fields-selectflag  = 'X'.
  append fields.
  fields-tabname     = 'CAUFV'.
  fields-fieldname  = 'AUART'.
  fields-selectflag  = ' '.
  append fields.
  fields-tabname     = 'CAUFV'.
  fields-fieldname  = 'WERKS'.
  fields-selectflag  = ' '.
  append fields.
  fields-tabname     = 'CAUFV'.
  fields-fieldname  = 'PLNBEZ'.
  fields-selectflag  = ' '.
  append fields.
  fields-tabname     = 'MAKT'.
  fields-fieldname  = 'MAKTX'.
  fields-selectflag  = ' '.
  append fields.

  select distinct a~aufnr a~auart a~werks a~gamng a~plnbez f~maktx
           into table ti_caufv
           from caufv     as a
          inner join afvc as b
             on b~aufpl   = a~aufpl
          inner join makt as f
             on f~matnr   = a~stlbez
          where b~arbid   > 0
            and f~spras   = sy-langu.

  loop at ti_caufv.
    valuetab-value = ti_caufv-aufnr.      append valuetab.
    valuetab-value = ti_caufv-auart.      append valuetab.
    valuetab-value = ti_caufv-werks.      append valuetab.
    valuetab-value = ti_caufv-plnbez.     append valuetab.
    valuetab-value = ti_caufv-maktx.      append valuetab.
  endloop.

  call function 'HELP_VALUES_GET_WITH_TABLE'
    exporting
      display                       = space
      show_all_values_at_first_time = 'X'
      titel                         = 'Número da Ordem'
    importing
      select_value                  = tl_aufnr
    tables
      fields                        = fields
      valuetab                      = valuetab.

  if sy-subrc ne 0.
    message e004(56) with 'Erro na seleção do campo'.
  endif.

endmodule.

****************************************************************************
Bapi alterar materiais (me02n).

* --------------------------------------------
* Variáveis & Contantes
* --------------------------------------------
  data: c_space(2) type c value '  ',

          c_x(1)        type c value 'X'.


* --------------------------------------------
* Work area
* --------------------------------------------
data: w_header     type bapimathead,

        w_material   type bapi_mara,      " Alterações dos campos
        w_materialx  type bapi_marax.   " Marca campos que sofrerão alteração


* --------------------------------------------
* Processamento
* --------------------------------------------
  w_header-material          = mara-matnr.  " Material a ser alterado

  w_material-pur_status    = c_space.       
  w_materialx-pur_status  = c_x.               

  call function 'BAPI_MATERIAL_SAVEDATA'
       exporting
            headdata    = w_header
            clientdata  = w_material
            clientdatax = w_materialx.


****************************************************************************
Abas na tela de seleção


SELECTION-SCREEN: BEGIN OF TABBED BLOCK t10 FOR 08 LINES,
                  TAB (35) l10 USER-COMMAND frame
                    DEFAULT SCREEN 100,
                  TAB (35) l15 USER-COMMAND cost
                    DEFAULT SCREEN 105,
                  TAB (35) l20 USER-COMMAND status
                    DEFAULT SCREEN 110.
SELECTION-SCREEN: END OF BLOCK t10.

SELECTION-SCREEN: BEGIN OF SCREEN 100 AS SUBSCREEN.
SELECT-OPTIONS:   tripno FOR  v_ptrv_appr-reinr,
                  datv1  FOR  v_ptrv_appr-datv1,
                  datb1  FOR  v_ptrv_appr-datb1,
                  kunde  FOR  v_ptrv_appr-kunde,
                  zort1  FOR  v_ptrv_appr-zort1,
                  zland  FOR  v_ptrv_appr-zland,
                  curr   FOR  ptrv_shdr-currency   MODIF ID off,
                  total  FOR  ptrv_shdr-trip_total MODIF ID off.
SELECTION-SCREEN: END OF SCREEN 100,
                  BEGIN OF SCREEN 105 AS SUBSCREEN.
SELECT-OPTIONS:   kostl  FOR  ptrv_scos-costcenter,
                  kokrs  FOR  ptrv_scos-co_area,
                  aufnr  FOR  ptrv_scos-internal_order,
                  kdauf  FOR  ptrv_scos-sales_ord,
                  kdpos  FOR  ptrv_scos-s_ord_item,
                  ps     FOR  ptrv_scos-wbs_elemt,
                  proj_id FOR  ptrv_scos-project_ext_id,  
                  task_id FOR  ptrv_scos-task_role_ext_id, 
                  nplnr  FOR  ptrv_scos-network.
SELECTION-SCREEN: END OF SCREEN 105,
                  BEGIN OF SCREEN 110 AS SUBSCREEN.
SELECT-OPTIONS:   antrg FOR ptk11-antrg,                  
                  abrec  FOR ptk11-abrec DEFAULT '0' TO '1',
                  ueblg  FOR ptk11-ueblg,
                  uebrf  FOR ptk11-uebrf,
                  uebdt  FOR ptk11-uebdt,
                  druck  FOR ptk11-druck.
SELECTION-SCREEN: END OF SCREEN 110.

initialization.

  l10 = 'Aba 1'(l10).
  l15 = 'Aba 2'(l15).
  l20 = 'Aba 3'(l20).

****************************************************************************
Informações do Usuário - EMAIL

Buscar os campos BNAME PERNSNUMBER na tabela USER_ADDRP
onde bname = Nome_usuario.

Depois buscar na tabela ADR6 o campo SMTP_ADDR onde
persnumber = USER_ADDRP-PERSNUMBER

USER_ADDRP-BNAME -> endereço de email INTERNO	
ADR6-SMTP_ADDR -> endereço de email EXTERNO

----------------------
Função que retorna o nome do usuário SAP
FKK_GET_USER_NAME

Retorna dados diversos do usuário: (nome, função, departamento, etc)
SUSR_USER_ADDRESS_READ
****************************************************************************
Dados empresa e local de negócios (tabela J_1BBRANCH) --- cgc --- cnpj

Visão: J_1BBRANCV.

****************************************************************************
Exibir PDF em tela

FORM create_pdf_control.
  DATA: l_pdf_path(255).

* Caminho do PDF
  CONCATENATE t_nfe-path
              '\'
              t_nfe-filename
         INTO l_pdf_path.

* Cria Container
  IF my_container IS INITIAL.
    CREATE OBJECT my_container
        EXPORTING
            container_name = 'PDF'
        EXCEPTIONS
            others = 1.
    IF sy-subrc <> 0.
      RAISE cntl_error.
    ENDIF.
  ENDIF.

* Cria Objeto para abertura de PDF
  IF pdf_control IS INITIAL.
    CREATE OBJECT pdf_control
         EXPORTING
              parent    = my_container.
    IF sy-subrc NE 0.
      RAISE cntl_error.
    ENDIF.
  ENDIF.

* Abre PDF
  IF sy-subrc EQ 0.
    CALL METHOD pdf_control->show_url
         EXPORTING
              url       = l_pdf_path.
  ENDIF.
ENDFORM.


programa standard SAP_PDF_VIEWER_DEMO

****************************************************************************
Descrição de mês (meses)

Função IDWT_READ_MONTH_TEXT

Ou ler:
      select single ltx into v_text_mes
             from t247
                  where spras = sy-langu and
                          mnr = wa_meses-spmon+4(2).

****************************************************************************
Chave desenvolvedor/objeto

DEVACCESS
ADIRACCESS

****************************************************************************
Price ---- conditions manuais

      LOOP AT xvbap WHERE updkz NE updkz_delete
                       AND posar NE charb
                       AND pvpos IS INITIAL.
        svbap-tabix = sy-tabix.
        PERFORM vbap_bearbeiten_vorbereiten(sapfv45p).
        PERFORM preisfindung_vorbereiten(sapfv45p) USING 'P'.
        tkomp-ix_vbap = svbap-tabix.
        APPEND tkomp.
      ENDLOOP.

    KOMV-KBETR  = L_VALUE1.
    KOMV-KSCHL  = t683v-kartv.
    RV45A-KOEIN = VBAK-WAERK.

* EAV 16/03/2009 - (WBS-292788) - Início
  DESCRIBE TABLE TKOMP LINES l_tkomp.
  DESCRIBE TABLE XVBAP LINES l_xvbap.
  l_inser = l_xvbap - l_index.
  IF l_inser GT 0.
  l_inser = l_tkomp - l_inser.
  l_inser = l_inser + 1.
  ELSEIF l_inser lt 0.
  l_inser =  0.
  ENDIF.
* EAV 16/03/2009 - (WBS-292788) - Fim

LOOP AT TKOMP from l_inser.

    CALL FUNCTION 'PRICING_MANUAL_INPUT'
           EXPORTING
                I_KOMK          = TKOMK
                I_KOMP          = TKOMP
                I_KSCHL         = KOMV-KSCHL
                I_KBETR         = KOMV-KBETR
                I_WAERS         = RV45A-KOEIN
                I_KRECH         = KOMV-KRECH
                I_Kpein         = KOMV-Kpein
                I_Kmein         = KOMV-Kmein
           IMPORTING
                E_KOMK          = TKOMK
                E_KOMP          = TKOMP
                E_KSCHL         = KOMV-KSCHL
                E_KBETR         = KOMV-KBETR
                E_WAERS         = RV45A-KOEIN
           TABLES
                TKOMV           = XKOMV
           EXCEPTIONS
                FIELD_INITIAL   = 1
                LINE_NOT_UNIQUE = 2
                CHECKS_FAILED   = 3
                OTHERS          = 4.

    ENDLOOP.

    l_netwr = vbak-netwr.
    perform preisfindung_gesamt using chara.
    vbak-netwr = l_netwr.

Ou:
      " Calcular e alterar a komv.
      CALL FUNCTION 'PRICING_MANUAL_INPUT'
        EXPORTING
          i_komk          = i_komk
          i_komp          = i_komp
          i_kschl         = i_kschl
          i_kbetr         = i_valor
          i_waers         = i_waers
        TABLES
          tkomv           = t_komv
        EXCEPTIONS
          field_initial   = 1
          line_not_unique = 2
          checks_failed   = 3
          OTHERS          = 4.

*******************************************************
Status gui standard (ALV)

Itens  1-  7             DESLB                 REJEIT                SHOW       &ETA


Itens  8- 14             &ALL       &SAL                  &OUP       &ODN       &ILT


Itens 15- 21             &UMC       &SUM       &RNT_PREV             &VEXCEL    &AQW       %PC


Itens 22- 28             %SL        &ABC       &GRAPH                &OL0       &OAD       &AVE


Itens 29- 35                        &INFO





Teclas de função               Desbloqueio de Alçadas

arra ferram.
           &DATA_SAVE BACK       CANC       EXIT       &RNT       %SC

*******************************************************
Gerar um nr randômico.

  DATA: l_senha TYPE datatype-char0128,
        l_num   TYPE datatype-integer2,
        l_index TYPE sy-tabix.

* Função para gerar texto de 6 caracteres
  CALL FUNCTION 'RANDOM_C_BY_SET'
    EXPORTING
      len_min   = 6
      len_max   = 6
      char_min  = 1
      char_max  = 50
      charset   = ''
    IMPORTING
      rnd_value = l_senha.

  MOVE l_senha TO p_senha.

* Para os campos que vierem em branco gera valor numérico
  DO 6 TIMES.

    l_index = sy-index - 1.

    IF p_senha+l_index(1) IS INITIAL.

      CALL FUNCTION 'RANDOM_I2'
        EXPORTING
          rnd_min   = 0
          rnd_max   = 9
        IMPORTING
          rnd_value = l_num.

      p_senha+l_index(1) = l_num.

    ENDIF.

  ENDDO.

*******************************************************

Duplo clique em table control

No status gui --> PF-STATUS ---> SET F2 = PICK

In PAI:-

MODULE USER_COMMAND_8001.
  CASE SY-UCOMM.
    WHEN 'PICK'.
      GET CURSOR FIELD v_fieldname Value v_fieldval.       
      "para pegar o 'sy-index'
      "get cursor line v_index.
      IF v_fieldname EQ 'VBELN'.
        "select query (in  where clause us v_fieldval to fetch associted records)
        SET SCREEN '8002'.
        LEAVE SCREEN.
      ENDIF.
  ENDCASE.
ENDMODULE.

*******************************************************

Atualizar fórmulas

RV80HGEN

*******************************************************
ZJ_BNFECALLRFC (gera nr nfe e envia XML para secretaria da fazenda)

Atualizar XBLNR, na bkpf, qdo transferências entre plantas (migo).

No final do form archive_link_barcode_update using doc_number, da função J_1B_NF_DOC_INSERT_FROM_OBJECT, criar um enhancement implícito: 
  "Atualização do campo 'XBLNR', nos lançamentos contábeis, qdo transferências entre plantas -- (Claudimiro)
  if ( NOT wk_header-nfe IS INITIAL )     "Se for NF eletrônica
         and ( 'ZMMM' ca wk_header-nftype ). "ZM - Nota Fiscal saída (Transferência)
                                               "MM - NF Saída (Transferencia) NF-e
     data: l_xblnr like bkpf-xblnr.

     clear l_xblnr.
     call function 'J_1B_NF_NUMBER_CONDENSE'
       exporting
         series     = wk_header-series
         subseries  = wk_header-subser
         nf_number9 = wk_header-nfenum
       importing
         ref_number = l_xblnr
       exceptions
         others     = 1.

     "Exporta o XBLNR pra memória, que será importado na badi DELIVERY_PUBLISH (implem ZTRANSF_PLANTA_XBLNR),
     "método PUBLISH_AFTER_SAVE,
     export zxblnr = l_xblnr to memory id 'ZNFENUM_XBLNR'.
  endif.
*> Luismar - 08/01/2009 - KFH-703579 - Fim


Na badi 'delivery_publish':

method if_ex_delivery_publish~publish_after_save.
*> Luismar - 08/01/2009 - KFH-703579 - Início
  "Atualização do campo 'XBLNR', nos lançamentos contábeis, qdo transferências entre plantas -- (Claudimiro)
  " Objetivo ......: Executar a FB02 e alterar o campo de referência(XBLNR),
  "                  caso importe da memória o id 'ZNFENUM_XBLNR'.
  data: v_xblnr type xblnr1.

  clear v_xblnr.
  "Importa da memória, o número da nf eletrônica/série para atualizar no campo XBLNR da FB02.
  "Este campo c/ nr nfe foi exportado pra memória qdo registrando saída mercadoria,
  "no include LJ1BBU14 (enhancement ZTRANSF_PLANTAS_XBLNR), somente qdo transferências entre plantas.
  import zxblnr = v_xblnr from memory id 'ZNFENUM_XBLNR'.

  check ( sy-subrc = 0 )   "Se importou com sucesso
       and ( not v_xblnr is initial ).  "Se nr nfe está preenchido

  data: begin of wa_transfp,
          bukrs type bkpf-bukrs,
          belnr type bkpf-belnr,
          gjahr type bkpf-gjahr,
        end of wa_transfp.

  clear wa_transfp.
  "Importa da memória (da função ZSAMPLE_INTERFACE_00001050) campos chave do lançamento contábil
  import ztransfp = wa_transfp from memory id 'ZTRANSFP'.

  "Verifica se campos estão preenchidos
  check ( sy-subrc = 0 )   "Se importou com sucesso
     and ( not wa_transfp is initial ).  "Checa se campos BELNR, BUKRS, GJAHR, estão preenchidos

  "Executa função que chama a FB02, para atualizar o campo de referência (XBLNR) com o nr da NFe
  call function 'ZFI_FB02'
    exporting
      i_bukrs = wa_transfp-bukrs
      i_belnr = wa_transfp-belnr
      i_gjahr = wa_transfp-gjahr
      i_xblnr = v_xblnr.

  free memory id 'ZTRANSFP'.
  free memory id 'ZNFENUM_XBLNR'.

*> Luismar - 08/01/2009 - KFH-703579 - Fim

endmethod.

*******************************************************

Hotspot write

DATA: input_field TYPE c LENGTH 100, 
      line_num TYPE i. 

START-OF-SELECTION. 
  WRITE 'Input text:'. 
  SET BLANK LINES ON. 
  FORMAT INPUT. 
  WRITE / input_field. 
  FORMAT INPUT OFF. 
  WRITE / '>>> OK <<<' COLOR 5 HOTSPOT. 

AT LINE-SELECTION. 
  IF sy-lisel = '>>> OK <<<'. 
    line_num = sy-lilli - 1. 
    READ LINE line_num FIELD VALUE input_field. 
    WRITE:   'The input was:', 
           /  input_field. 

*******************************************************
Autorizar Debug:

Acessar a transação su01... clicar no lapis para modificar e depois na aba perfis.

Colocar esses dois perfis:
SAP_ALL
SAP_NEW

*******************************************************

SELECTION-SCREEN BEGIN OF LINE.
PARAMETER rb1 RADIOBUTTON GROUP r1.
SELECTION-SCREEN COMMENT 10(10) text-001 FOR FIELD RB1.
SELECTION-SCREEN END OF LINE.
 
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 10.
PARAMETER rb11 RADIOBUTTON GROUP r2.
SELECTION-SCREEN COMMENT 22(20) text-021 FOR FIELD RB11.
SELECTION-SCREEN END OF LINE.
 
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 10.
PARAMETER rb12 RADIOBUTTON GROUP r2.
SELECTION-SCREEN COMMENT 22(20) text-022 FOR FIELD RB12.
SELECTION-SCREEN END OF LINE.
 
SELECTION-SCREEN BEGIN OF LINE.
PARAMETER rb2 RADIOBUTTON GROUP r1.
SELECTION-SCREEN COMMENT 10(10) text-002 FOR FIELD RB2.
SELECTION-SCREEN END OF LINE.
 
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 10.
PARAMETER rb21 RADIOBUTTON GROUP r3.
SELECTION-SCREEN COMMENT 22(20) text-021 FOR FIELD RB21.
SELECTION-SCREEN END OF LINE.
 
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION 10.
PARAMETER rb22 RADIOBUTTON GROUP r3.
SELECTION-SCREEN COMMENT 22(20) text-022 FOR FIELD RB22.
SELECTION-SCREEN END OF LINE.

*******************************************************

SELECTION-SCREEN BEGIN OF BLOCK 1 WITH FRAME TITLE text-001.
PARAMETERS: p_fisi RADIOBUTTON GROUP tipo USER-COMMAND tipo,
            p_juri RADIOBUTTON GROUP tipo.
SELECTION-SCREEN BEGIN OF BLOCK 2 WITH FRAME TITLE text-002.
PARAMETERS: p_cpf TYPE c LENGTH 15,
            p_cnpj TYPE c LENGTH 30.
SELECTION-SCREEN END OF BLOCK 2.
SELECTION-SCREEN END OF BLOCK 1.

INITIALIZATION.
  p_fisi = 'X'.

AT SELECTION-SCREEN OUTPUT.
  LOOP AT SCREEN.
    IF p_fisi = 'X'.
      IF screen-name = 'P_CNPJ' OR screen-name = '%_P_CNPJ_%_APP_%-TEXT'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
    ELSE.
      IF screen-name = 'P_CPF' OR screen-name = '%_P_CPF_%_APP_%-TEXT'.
        screen-active = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDIF
..
  ENDLOOP.

*******************************************************

* Verifica se programa foi chamado por variante da transação (SHD0)


  data: wa_tcvariant like shdtvciu-tcvariant.
  clear wa_tcvariant.

  "Verifica se para a transação corrente, existe uma variante de transação.
  CALL FUNCTION 'RS_HDSYS_GET_TC_VARIANT'
       IMPORTING
            tcvariant = wa_tcvariant.

  if ( not wa_tcvariant is initial )               "se campo está preenchido, é porquê existe uma variante de transação...


Ou ----------

  data: v_var(20)       type c,

  clear v_var.

  call 'DY_GET_TX_VARIANT'
    id 'VARIANT' field v_var.
  if v_var is initial.
    if not sy-tcode eq 'START_REPORT'.
      v_var = sy-tcode.
    else.
      v_var = sy-slset.
    endif.
  endif.

  if v_var = 'ZZSD122'.
      ...
  endif.

*******************************************************
Função pra buscar tabela de constantes:

ranges: r_auart_lj for komk-auart_sd.

      clear: r_auart_lj[].
      refresh: r_auart_lj[].
      call function 'ZGE001TF01'
        exporting
          i_programa      = 'RV64A950'
          i_constantes    = 'AUART'
        tables
          t_valores_const = r_auart_lj
        exceptions
          const_not_found = 1
          const_empty     = 2
          others          = 3.

*******************************************************
Encontrar badi em transações standards:

1. Va na transação se24, visualize a classe CL_EXITHANDLER;
2. Quando aparecer os metodos, de 2 click's no metodo GET_INSTANCE;
3. Poe uma break no metodo CALL METHOD cl_exithandler=>get_class_name_by_interface, esse metodo retorna o nome na classe ( class_name ) e o da BADI dessa classe ( exit_name ) 

*******************************************************
Converter valores (moeda)

CONVERT_TO_FOREIGN_CURRENCY

*******************************************************
READ TABLE dinâmico:

READ TABLE tabela
    INTO workarea
    WITH KEY ('CAMPO1') = 'valor1'
                     ('CAMPO2') = 'valor2'.

----
select dinâmico:
data:
   w_campos     type string,
   w_tblname    type string,
   w_where      type string.

*   Campo a ser selecionado
    w_campos = wa_zbcat0007-campo.

*   Tabela a ser lida
    w_tblname = wa_zbcat0007-tabela.

*   Condições (where) para a leitura
    concatenate 'bkkrs = ''' i_bkkrs ''''
                  ' AND docno = ''' i_docno ''''
                  ' AND posno = ''' wa_zbcat0007-posno ''''
                  into w_where.

*   Lê a tabela preparada
    select single (w_campos) into w_valor
           from (w_tblname)
                where (w_where).
*******************************************************
Função OIL_LAST_DAY_OF_PREVIOUS_MONTH:
Retorna último dia de um mês anterior à uma data informada
*******************************************************
Através do período, monta data início e fim:

PARAMETERS: month LIKE marv-lfmon OBLIGATORY,
            lfgja LIKE marv-lfgja OBLIGATORY.

data: periv LIKE t001-periv.
DATA: poper like t009b-poper,
      datum_low     LIKE sy-datum,
      datum_high    LIKE sy-datum.

  SELECT SINGLE periv FROM t001 INTO periv WHERE bukrs = bukrs.
  poper = month.
  CALL FUNCTION 'FIRST_DAY_IN_PERIOD_GET'
    EXPORTING
      I_GJAHR = lfgja
      I_PERIV = PERIV
      I_POPER = poper
    IMPORTING
      E_DATE  = datum_low
    EXCEPTIONS
      INPUT_FALSE          = 1
      T009_NOTFOUND        = 2
      T009B_NOTFOUND       = 3
      OTHERS               = 4.

   IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
   ENDIF.

  CALL FUNCTION 'LAST_DAY_IN_PERIOD_GET'
    EXPORTING
      I_GJAHR = lfgja
      I_PERIV = PERIV
      I_POPER = poper
    IMPORTING
      E_DATE  = datum_high
    EXCEPTIONS
      INPUT_FALSE          = 1
      T009_NOTFOUND        = 2
      T009B_NOTFOUND       = 3
      OTHERS               = 4.

   IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
   ENDIF.                                                  " note 720396

*******************************************************
Retorna próximo dia útil:
- Usando nr de dias na passagem da função:
      CALL FUNCTION 'BKK_ADD_WORKINGDAY'
        EXPORTING
          i_date      = sy-datum
          i_days      = vg_dias_uteis
          i_calendar1 = 'BR'
        IMPORTING
          e_date      = vg_vcto_dias_uteis
          e_return    = vg_return1.

- Não utilizando nr de dias na função. Se a data informada não for dia útil, retorna próxima data.
          call function 'BKK_GET_NEXT_WORKDAY'
            exporting
              i_date         = p_dtprev
              i_calendar1    = 'BR'
            importing
              e_workday      = p_dtprev
            exceptions
              calendar_error = 1
              others         = 2.


- Pegar o dia da semana correspondente a data:
data: l_day_week type p.

          call function 'DAY_IN_WEEK'
            exporting
              datum = p_dtprev
            importing
              wotnr = l_day_week.

*******************************************************
SE73 - código de barra smartform
 0000645158 New bar code technology for Smart Forms

*************************************
NFE Status de Envio ao Sefaz:

100 - Homologação
101 - Cancelamento
102 - Inutilização

tabela J_1BSTSCODET há a descrição e numero de todos os status da SEFAZ

*******************************************************
SCU3 - Transação que mostra as tabelas que gravam log de modificação/modificações
Tabela DD09L (campo PROTOKOLL).

RSVTPROT (programa para exibir log modificação em tabelas onde está marcado para gravar log modificação (opções técnicas)).
Obs: Verificar include RSVTPF01, FORM check_authority (Na verdade, pra funcionar a pesquisa, tem-se que criar tela de diálogo pela SE11),
     assim vai criar um registro na tabela 'tddat', a qual é checada nesta include e form.
Pode-se fazer o seguinte: Copie o programa RSVTPROT para um Z, e retire a rotina de check auhority (linha 393 a 402).

-----
cdhdr (Cabeçalho do documento de modificação)
cdpos (Itens doc.modificação)
Na tabela TCDOB, pode-se verificar quais tabelas tem objeto de modificação.
Encontra-se nesta tabela também, a Classe de objetos (para a cdhdr ---- campo OBJECTCLAS).

CHANGEDOCUMENT_READ_HEADERS
CHANGEDOCUMENT_READ_POSITIONS

----------
dbtablog (Registros de log de sistemas para modificações de tabela Z)
Para chamar programa que mostra log modificações (tabela dbtablog)
  RANGES: sel_obj FOR objh-objectname,
          sel_type FOR objh-objecttype.

  SUBMIT rsvtprot VIA SELECTION-SCREEN USING SELECTION-SCREEN 1010
                  WITH cusobj IN sel_obj             "IEQZSDT001   (aqui vai o nome da tabela)
                  WITH stype IN sel_type             "IEQS   ('S')
                  WITH acc_arch = ' ' AND RETURN.

Ou:
Função DBLOG_READ_ARCHIVES

Pass the tables that you want to read in the patameter OBJ_LIST

Ou, funções:
DBLOG_READ
DBLOG_READ_WITH_STATISTIC
---------------------------


------- Início
Atualizar tabela log modificação por função:

Exemplo (atualizando log modificações de 2 novos campos da tabela VBAK):

data: wa_vkold type vbak,
      wa_vknew type vbak.

      " gravação para log
  wa_vkold-zzfrgke = '0'.
  wa_vknew-zzfrgke = '1'.


  data vl_objectid type cdhdr-objectid.

  if not wa_vknew is initial.
    " grava log na VA
    vl_objectid = p_vbeln.
    call function 'CHANGEDOCUMENT_OPEN'
      exporting
        objectclass = 'VERKBELEG'
        objectid    = vl_objectid.

    call function 'CHANGEDOCUMENT_SINGLE_CASE'
      exporting
        change_indicator = 'U'
        tablename        = 'VBAK'
        workarea_new     = wa_vknew
        workarea_old     = wa_vkold.

    call function 'CHANGEDOCUMENT_CLOSE'
      exporting
        date_of_change = sy-datum
        objectclass    = 'VERKBELEG'
        objectid       = vl_objectid
        tcode          = sy-tcode
        time_of_change = sy-uzeit
        username       = sy-uname.
  endif.
------- Fim

*******************************************************

Nota SAP 1179791 (Disparador da DANFE) nfe.

*******************************************************
Executa um programa em JOB, e aguarda que este, termine.

  data: vl_number        type tbtcjob-jobcount,
        vl_name          type tbtcjob-jobname,
        wa_params        type btcselect,
        wa_joblist       type tbtcjob,
        it_joblist       type standard table of tbtcjob,
        r_blart          type range of blart with header line,
        r_datum          type range of datum with header line.

  vl_name = 'ZFIJ15_FATURA_GNRE_SECR_FAZ'.

*> Na primeira vez busca um job em execução, senão existir, criar um job.
*> Na segunda  vez em diante verificar o encerramento do job para
*continuar
*> o processamento

  do.
    clear wa_params.
    wa_params-jobname  = vl_name.
    if ( sy-index eq 1 ).
      wa_params-running  = 'X'.
    else.
      wa_params-jobcount = vl_number.
      wa_params-finished = 'X'.
      wa_params-aborted  = 'X'.
    endif.

    call function 'BP_JOB_SELECT'
      exporting
        jobselect_dialog  = 'N'
        jobsel_param_in   = wa_params
      tables
        jobselect_joblist = it_joblist
      exceptions
        others            = 99.

    read table it_joblist into wa_joblist index 1.

    if ( sy-subrc eq 0 ).
      if ( sy-index eq 1 ).
        vl_number = wa_joblist-jobcount.
      else.
        exit.
      endif.
    else.
      if ( sy-index eq 1 ).

        call function 'JOB_OPEN'
          exporting
            jobname          = vl_name
          importing
            jobcount         = vl_number
          exceptions
            cant_create_job  = 1
            invalid_job_data = 2
            jobname_missing  = 3
            others           = 4.

        if ( sy-subrc eq 0 ).
          refresh: r_blart, r_datum.
          r_blart-sign   = 'I'.
          r_blart-option = 'EQ'.
          r_blart-low    = 'RV'.
          append r_blart.

          r_datum-sign   = 'I'.
          r_datum-option = 'EQ'.
          r_datum-low    = sy-datum.
          append r_datum.

          submit zfib006
            with p_bukrs = p_bukrs
            with so_blart in r_blart
            with so_cpudt in r_datum
            via job vl_name number vl_number
            and return.

          if ( sy-subrc eq 0 ).
            call function 'JOB_CLOSE'
              exporting
                jobcount             = vl_number
                jobname              = vl_name
                strtimmed            = 'X'
              exceptions
                cant_start_immediate = 1
                invalid_startdate    = 2
                jobname_missing      = 3
                job_close_failed     = 4
                job_nosteps          = 5
                job_notex            = 6
                lock_failed          = 7
                others               = 8.

          endif.
        endif.
      endif.
    endif.

  enddo.

*******************************************************

Verificar se programa está sendo executado em JOB, por programa.

data: begin of wa_jobs,
         jobcount     type tbtcp-jobcount,
         jobname      type tbtcp-jobname,
         progname     type tbtcp-progname,
      end   of wa_jobs.

data: it_jobs           like standard table of wa_jobs,
      vg_lin            type i.

  select distinct t2~jobcount t2~jobname t1~progname
          from tbtcp as t1
    inner join tbtco as t2
            on t2~jobcount eq t1~jobcount
           and t2~status  eq 'R'  "Running (Ativo)
    into table it_jobs
         where ( t1~progname eq sy-repid ).

  describe table it_jobs lines vg_lin.

  if ( vg_lin gt 1 ). " Verifica se há mais de um job em execução
    stop.
  elseif ( vg_lin gt 0 ). " Existe um job em execução então...
    if ( sy-batch ne 'X' ). " qdo o program é on-line aborta
      stop.
    else.
      " esta sessão do program é o job.
    endif.
  endif.

*******************************************************
MiniSap no windows Vista:
Executar no prompt do DOS.
Va até o CMD, clique com o botão direito, executar como administrador, após abrir o DOS jogue 
o comando CD\ para ir para a raiz do c: , digite cd miniwas, depois digite start runwa1,
 irá excutar o MINISAP, após terminar de executar o MINISAP abra o SAPGui e execute-o (Normamente da erro na 1ª vez), caso apareça uma de erro é só executar novamente o SAPGui novamente.

*******************************************************
DATA: t_bnflin  TYPE TABLE OF j_1bnflin,
      wa_bnflin TYPE j_1bnflin.

FIELD-SYMBOLS <bnflin> TYPE any table.

REFRESH: t_bnflin.
ASSIGN ('(ZJ_1BNFPR)wk_item[]')  TO <bnflin>.

if <bnflin> is assigned.

  t_bnflin[] = <bnflin>.
  loop at t_bnflin into wa_bnflin.
       wa_bnflin-meins = 'CX'.   "Altera a unidade de medida
     modify t_bnflin into wa_bnflin.
  endloop.

  <bnflin> = t_bnflin[].   "Atualiza os registros modificados no field-symbol

endif.

*************************************************************
Busca o layout ALV de um programa e retorna em tabela interna

  data: begin of wa_arquivo,
           linha(5000),
          end of wa_arquivo.

  data: v_caminho      TYPE string,
        v_ok(01)       type c,
        t_fieldcat     TYPE slis_t_fieldcat_alv,
        t_fieldcat1    TYPE slis_t_fieldcat_alv,
        s_fieldcat     type slis_fieldcat_alv,
        s_layout       type slis_layout_alv,
        wa_variant     LIKE disvariant,
        t_arquivo      like table of wa_arquivo with header line,
        v_campo(40),
        v_valor(100).

REUSE_ALV_VARIANT_SELECT

    if NOT sy-batch IS INITIAL.
      t_fieldcat1[] = fieldcat[].
      if NOT P_VARI IS INITIAL.
        move: sy-CPROG to wa_variant-report,
              'FLAT'   to wa_variant-handle,
              p_vari   to wa_variant-variant.

        "Verificar existencia do 'layout'
        call function 'REUSE_ALV_VARIANT_EXISTENCE'
          exporting
            i_save     = 'A'
          changing
            cs_variant = wa_variant.
      else.
        clear: wa_variant.
        wa_variant-report = sy-repid.
      endif.

      s_layout-colwidth_optimize = 'X'.
      s_layout-zebra             = 'X'.
      s_layout-window_titlebar   = 'Estoque de depósito'.

      call function 'REUSE_ALV_VARIANT_SELECT'
         exporting
              i_dialog            = ' '
              i_user_specific     = 'X'
              it_default_fieldcat = t_fieldcat1
              i_layout            = layout
         importing
              et_fieldcat         = t_fieldcat
              es_layout           = layout
         changing
              cs_variant          = wa_variant
         exceptions
              wrong_input         = 1
              fc_not_complete     = 2
              not_found           = 3
              program_error       = 4
              others              = 5.
      if sy-subrc ne 0.
        t_fieldcat[] = t_fieldcat1[].
      endif.
      sort t_fieldcat by col_pos.
      delete t_fieldcat where no_out eq 'X'.

      clear: v_ok.
      loop at t_fieldcat into s_fieldcat.
        if s_fieldcat-seltext_s IS INITIAL.
           s_fieldcat-seltext_s = s_fieldcat-fieldname.
        ENDIF.
        if not v_ok is initial.
          concatenate t_arquivo-linha
                      s_fieldcat-seltext_s into
                      t_arquivo-linha      separated by ';'.
        else.
          v_ok = 'X'.
          t_arquivo-linha  = s_fieldcat-seltext_s.
        endif.
      endloop.
      append t_arquivo.
      clear: v_ok.
    ENDIF.

*******************************************************
Layout de ALV em tela de seleção:

data: gt_variant like disvariant,
      h_exit,
      gx_variant like gt_variant.

selection-screen begin of block bl4 with frame.
parameters: p_varian like disvariant-variant.  "Layout de saída
selection-screen end of block bl4.


at selection-screen on value-request for p_varian.
  perform f_selecionar_layout using p_varian.

at selection-screen on p_varian.
  perform validacao_variante.

initialization.

   perform f_inicializar.


start-of-selection.

    gt_variant-variant = p_varian.

    call method grid1->set_table_for_first_display
      exporting
        is_variant           = gt_variant
        i_save               = 'A'
        is_layout            = gs_layout
        it_toolbar_excluding = t_exclude
      changing
        it_fieldcatalog      = t_dinam
        it_sort              = gt_sort
        it_outtab            = <dyn_table>.


form f_inicializar.

  clear gt_variant.
  gt_variant-report = sy-repid.

  call function 'REUSE_ALV_VARIANT_DEFAULT_GET'
    exporting
      i_save        = 'A'
    changing
      cs_variant    = gt_variant
    exceptions
      wrong_input   = 1
      not_found     = 2
      program_error = 3
      others        = 4.

  if sy-subrc = 0.
    p_varian = gt_variant-variant.
  endif.

endform.

form f_selecionar_layout using p_variant type any.

  clear gx_variant.

  call function 'REUSE_ALV_VARIANT_F4'
    exporting
      is_variant    = gt_variant
      i_save        = 'A'
    importing
      e_exit        = h_exit
      es_variant    = gx_variant
    exceptions
      not_found     = 1
      program_error = 2
      others        = 3.

  if sy-subrc is initial and h_exit is initial.
    gt_variant-variant = gx_variant-variant.
    p_variant          = gx_variant-variant.
  else.
    message id sy-msgid type 'S'
                 number sy-msgno
                 with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  endif.

endform.

form f_validacao_variante.

  if not p_varian is initial.
    move: gt_variant to gt_x_variant,
          p_varian   to gt_x_variant-variant.

*   Verificar existencia
    call function 'REUSE_ALV_VARIANT_EXISTENCE'
      exporting
        i_save     = 'A'
      changing
        cs_variant = gt_x_variant.
    gt_variant = gt_x_variant.
  else.
    clear: gt_variant.
    gt_variant-report = sy-repid.
  endif.

endform.

*******************************************************
Transação SAAB (Pontos de controle ativáveis):

  BREAK-POINT                ID MMIM_REP_MB5B. 

*******************************************************
Retorna plano de conta montado (textos)
     CALL FUNCTION 'FI_IMPORT_BALANCE_SHEET_TEXT'
        EXPORTING
          sprache        = bilaspra
          version        = t011-versn   "HYPM
        TABLES
          x011q          = x011q
        EXCEPTIONS
          text_not_found = 04.

*******************************************************
Rotinas bloqueio de objetos (com request que não subiu pra PRD)
se37 (fora - primeira tela)
SAPLSFUNCTION_BUILDER          / LSFUNCTION_BUILDERO02
FORM                           / OK_CODE_INIT
-----:

ENHANCEMENT 3  ZENHANC_EDIT_SE37.    "active version

"Luismar (31/08/2009) verificar se objeto está bloqueado (não subiu pra PRD) - Início
if ( sy-ucomm  = 'WB_EDIT' )
       and ( sy-tcode  = 'SE37' )
            and ( not rs38l-name is initial ).

  data: l_obj_name type progname,
        l_group    type RS38L_AREA, "Grupo de funções
        l_msg      type char100.

  "Busca o grupo de funções ao qual pertence o módulo de função
  clear l_group.
  CALL FUNCTION 'FUNCTION_INCLUDE_INFO'
       CHANGING
            FUNCNAME            = RS38L-NAME
            GROUP               = L_GROUP
       EXCEPTIONS
            FUNCTION_NOT_EXISTS = 1
            INCLUDE_NOT_EXISTS  = 2
            GROUP_NOT_EXISTS    = 3
            NO_SELECTIONS       = 4
            NO_FUNCTION_INCLUDE = 5
            OTHERS              = 6.

  if sy-subrc = 0.

  "Executa função que verifica se o objeto o qual se deseja editar,
  "possui request que ainda não subiu pra PRODUÇÃO.
    l_obj_name = l_group.
    clear l_msg.
    call function 'ZFBC_CHECA_OBJ_BLOQ'
      exporting
        i_obj_name = l_obj_name
      importing
        e_msg      = l_msg.

    if not l_msg is initial.
      "Possui request que ainda não subiu pra PRODUÇÃO.
      message e000(zbc) with l_msg(26) l_msg+26(40).
    endif.

  endif.

endif.
"Luismar (31/08/2009) verificar se objeto está bloqueado (não subiu pra PRD) - Fim

-------------------
se37 (dentro)
SAPLSFUNCTION_BUILDER          / LSFUNCTION_BUILDERF01
FORM                           / OK_CODE_TABSTRIP

ENHANCEMENT 1  ZENHANC_EDIT2_SE37.    "active version
"Luismar (31/08/2009) verificar se objeto está bloqueado (não subiu pra PRD) - Início
if ( sy-ucomm  = 'WB_DISP_EDIT_TOGGLE' )
       and ( sy-tcode  = 'SE37' )
            and ( not rs38l-name is initial )
                  and ( HEADER-ACTION =   'SHOW' ).

  data: l_obj_name type progname,
        l_group    type RS38L_AREA, "Grupo de funções
        l_msg      type char100.

  "Busca o grupo de funções ao qual pertence o módulo de função
  clear l_group.
  CALL FUNCTION 'FUNCTION_INCLUDE_INFO'
       CHANGING
            FUNCNAME            = RS38L-NAME
            GROUP               = L_GROUP
       EXCEPTIONS
            FUNCTION_NOT_EXISTS = 1
            INCLUDE_NOT_EXISTS  = 2
            GROUP_NOT_EXISTS    = 3
            NO_SELECTIONS       = 4
            NO_FUNCTION_INCLUDE = 5
            OTHERS              = 6.

  if sy-subrc = 0.

  "Executa função que verifica se o objeto o qual se deseja editar,
  "possui request que ainda não subiu pra PRODUÇÃO.
    l_obj_name = l_group.
    clear l_msg.
    call function 'ZFBC_CHECA_OBJ_BLOQ'
      exporting
        i_obj_name = l_obj_name
      importing
        e_msg      = l_msg.

    if not l_msg is initial.
      "Possui request que ainda não subiu pra PRODUÇÃO.
      message i000(zbc) with l_msg(26) l_msg+26(40).
      exit.
    endif.

  endif.

endif.
"Luismar (31/08/2009) verificar se objeto está bloqueado (não subiu pra PRD) - Fim

-------------------
SE38 --- user exit EXIT_SAPLS38E_001
"Luismar (31/08/2009) verificar se objeto está bloqueado (não subiu pra PRD) - Início
if operation = 'EDIT'.

  data: v_msg type char100.

  "Executa função que verifica se o objeto o qual se deseja editar,
  "possui request que ainda não subiu pra PRODUÇÃO.
  clear v_msg.
  call function 'ZFBC_CHECA_OBJ_BLOQ'
    exporting
      i_obj_name = program
    importing
      e_msg      = v_msg.

  if not v_msg is initial.
    "Possui request que ainda não subiu pra PRODUÇÃO.
    message i000(zbc) with v_msg raising cancelled.
  endif.

endif.
"Luismar (31/08/2009) verificar se objeto está bloqueado (não subiu pra PRD) - Fim


*******************************************************
Altera texto parameter ou select-options (tela de seleção):

%_SO_BUPLA_%_APP_%-TEXT = 'Local de negócios'.

*******************************************************
Mensagens Variáveis ( SD )

MODIFICAR TIPO DA MENSAGEM VARIAVEL NA TRANSAÇÃO SAP ( SD )

W (Alerta/Amarela) / E (Erro/Vermelha) / I (Informativa/Verde)

Em alguns casos é interessante que o processo seja bloqueado apesar de uma mensagem

de alerta (amarela) ser emitida. Neste caso pode-se alterar o tipo da mensagem de Alerta

para Erro (vermelha) e o contrário também é possível.


Detalhe: Deve ser feito no ambiente de configuração para gerar request


Caminho no customizing:

Vendas e distribuição / Venda / Documentos de vendas / Definir mensagens variáveis

Transação: OVAH

*******************************************************
Validar campos tela de seleção FBL5N:

form sel_account_check.  (enhancement implícito, no início form)
ENHANCEMENT 1  Z_ENHAC_VALIDA_CAMPOS.    "active version
  "============ Validar tela de seleção da FBL5N...
  if ( sy-tcode = 'FBL5N' )
               and ( sy-batch is initial ).   "Se executando online...

     "Valida empresa...
     if dd_bukrs[] is initial.
        message e000(zfi) with 'Favor informar a empresa!'.
     endif.

  endif.
ENDENHANCEMENT.

*******************************************************
Novos campos na FBL5N

Appendar campo na estrutura RFPOSXEXT (exemplo abaixo: zzcpudt).

Enhancement implícito (Z_RFITEMAR_NOVOCAMPO) no:
Programa:RFITEMAR  Form:EXPORT_FILITEXTS_DATA

  data: begin of it_bkpf occurs 0,
          bukrs type bkpf-bukrs,
          belnr type bkpf-belnr,
          gjahr type bkpf-gjahr,
          cpudt type bkpf-cpudt,
        end of it_bkpf.

  if it_pos[] is not initial.

     select bukrs belnr gjahr cpudt
       from bkpf
       into table it_bkpf
        for all entries in it_pos
      where bukrs eq it_pos-bukrs
        and belnr eq it_pos-belnr
        and gjahr eq it_pos-gjahr.

     sort it_bkpf by bukrs belnr gjahr.

     loop at it_pos.

       read table it_bkpf with key bukrs = it_pos-bukrs
                                   belnr = it_pos-belnr
                                   gjahr = it_pos-gjahr
                                   binary search.

       if sy-subrc eq 0.
          it_pos-zzcpudt = it_bkpf-cpudt.
          modify it_pos.
       endif.

     endloop.

   endif.

----------------
Tabelas variante:

VARID  --- VARIT  --- VARIS --- VALUTAB
RS_VARIANT_LIST --- RS_VARIANT_DISPLAY  --- RS_VARIANT_CONTENTS (SE37)
RS_VARIANT_VALUES (SE38)

Verifica se variante existe:
    CALL FUNCTION 'RS_VARIANT_EXISTS'

Salva:
    CALL FUNCTION 'RS_CREATE_VARIANT'

popup com variantes do programa:
function RS_VARIANT_CATALOG

Busca parâmetros da variante do programa (retorna tabela interna com campos e valores):
    CALL FUNCTION 'RS_VARIANT_VALUES_TECH_DATA'

Deleta:
 CALL FUNCTION 'RS_VARIANT_DELETE'

*******************************************************
Search help (tela de pesquisa) de variantes de um programa qualquer.

DATA: SEL_VARIANT LIKE CODIA-VARIANT.

  CALL FUNCTION 'RS_VARIANT_CATALOG'
       EXPORTING
            REPORT      = 'RKOSEL00'
            NEW_TITLE   = TEXT-001
            variant     = codia-variant
       IMPORTING
            SEL_VARIANT = SEL_VARIANT.

  IF NOT SEL_VARIANT IS INITIAL.
    p_varian = SEL_VARIANT.
  endif.
*******************************************************

  FIELD-SYMBOLS: <F>.

  lt_auf é uma tabela interna.

  ASSIGN COMPONENT 'OBJNR' OF STRUCTURE LT_AUF TO <F>.

    LOOP AT LT_AUF.
      ONRTAB-OBJNR = <F>.
      APPEND ONRTAB.
    ENDLOOP.



transação 
tables: codia.

DATA: BEGIN OF MEMTAB OCCURS 0.
        INCLUDE STRUCTURE COSEL1.
DATA: END OF MEMTAB.

data: MEMORY_ID(32) VALUE 'exported_from_RKOSEL00_or_KOSM'.
DATA: SEL_VARIANT LIKE CODIA-VARIANT.

data: begin of T_AUFK OCCURS 0.
      INCLUDE STRUCTURE AUFK.
data: end of T_AUFK.

parameters: p_varian LIKE CODIA-VARIANT OBLIGATORY.

at selection-screen on value-request for p_varian.
  perform f_variantes.

start-of-SELECTION.

    "Variantes da transação 'kok5'
    perform f_dados_variante.

    "Executa um form num programa standard e retorna dados de ordens    
    PERFORM SELECT_ORDERS(RKOSEL00) TABLES memtab
                                           t_AUFk.


form f_variantes.

  CALL FUNCTION 'RS_VARIANT_CATALOG'
       EXPORTING
            REPORT      = 'RKOSEL00'
            NEW_TITLE   = TEXT-001
            variant     = codia-variant
       IMPORTING
            SEL_VARIANT = SEL_VARIANT.

  IF NOT SEL_VARIANT IS INITIAL.
    p_varian = SEL_VARIANT.
    perform f_dados_variante.
  ENDIF.

ENDFORM.

form f_dados_variante.

    REFRESH MEMTAB.
*   export space to memory id memory_id.
    FREE MEMORY ID MEMORY_ID.
    SUBMIT RKOSEL00 USING SELECTION-SET p_VARIAN AND RETURN.
    IMPORT CODIA-VARIANT MEMTAB FROM MEMORY ID MEMORY_ID.
    SET PARAMETER ID 'ORV' FIELD p_varian.

endform.
"Busca as ordens
    PERFORM SELECT_ORDERS(RKOSEL00) TABLES LOCTAB
                                           LT_AUF.


************************************************
BAPI_SALESORDER_GETSTATUS

Busca quantidades (fornecidas) de ordem de venda

*******************************************************
uma das formas de ler tabelas de outro ambiente SAP, é usando a função:

RFC_READ_TABLE

Com ela e a destination, você acessa tabelas em outro ambiente SAP.

--- Programa exemplo: ------------------ Início
report  ytest.
tables: j_1bnfdoc.

parameters: rfc_sys   type rfcdoc-rfcdest default ' '  obligatory .  "Conexão SAP, criada na transação SM59
parameters: p_pstdat   type j_1bnfdoc-pstdat.

data: line            like line .
data: t_retorno_dados like tab512 occurs 100000 with header line.  "Aqui pode-se usar qualquer estrutura (por exemplo, a própria tabela a ser lida)

data: options1    like rfc_db_opt  occurs     10 with header line.
data: t_fields    like rfc_db_fld  occurs     10 with header line.
data: begin of i_langobj occurs 20,
        id like dokil-id          ,
      end of i_langobj            .

data: begin of wa_j1bnfdoc,
        docnum  type j_1bnfdoc-docnum,
        pstdat  type j_1bnfdoc-pstdat,
        crenam  type j_1bnfdoc-crenam,
      end of wa_j1bnfdoc.

data: t_j1bnfdoc like TABLE OF wa_j1bnfdoc WITH HEADER LINE.


start-of-selection.

  "Condição 'where' da leitura da tabela --- Início
  concatenate 'PSTDAT = '''
              p_pstdat
              ''''
        into  line.
  move: line to options1 .
  append options1 .
  "Condição 'where' da leitura da tabela --- Fim

  "Campos a retornar da leitura na tabela --- Início
  perform f_grava_fields using 'DOCNUM'   '001'  '10'   'N'.
  perform f_grava_fields using 'PSTDAT'   '011'  '8'    'D'.
  perform f_grava_fields using 'CRENAM'   '019'  '12'   'C'.

  "Campos a retornar da leitura na tabela --- Fim

  call function 'RFC_READ_TABLE'
       destination                rfc_sys
       exporting
            query_table          = 'J_1BNFDOC'
             DELIMITER            = '|'
       tables
            options              = options1
            fields               = t_fields
            data                 = t_retorno_dados
       exceptions
            table_not_available  = 1
            table_without_data   = 2
            option_not_valid     = 3
            field_not_valid      = 4
            not_authorized       = 5
            data_buffer_exceeded = 6
            others               = 7 .

  if sy-subrc = 0.
    "Lê os registros da tabela lida na função.
    perform f_le_retorno.
  endif.

endform.                    "f_dados_hypm

*&---------------------------------------------------------------------*
*&      Form  f_le_retorno
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form f_le_retorno.

  check not t_retorno_dados[] is initial.

  "Lê os registros da tabela lida na função e armazena em tabela de impressão.
  clear t_j1bnfdoc[].
  refresh t_j1bnfdoc[].
  loop at t_dados.
    split t_dados-wa at '|' into wa_j1bnfdoc-docnum wa_j1bnfdoc-pstdat wa_j1bnfdoc-crenam.
    append wa_j1bnfdoc to t_j1bnfdoc.
  endloop.

endform.

*&---------------------------------------------------------------------*
*&      Form  f_grava_fields
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_CAMPO    text
*      -->P_OFFSET   text
*      -->P_LENGTH   text
*      -->P_TYPE     text
*----------------------------------------------------------------------*
form f_grava_fields using p_campo p_offset p_length p_type.

  clear t_fields.
  t_fields-fieldname = p_campo.
  t_fields-offset = p_offset.
  t_fields-length = p_length.
  t_fields-type = p_type.
  append t_fields.

endform.                    "f_grava_fields

--- Programa exemplo: ------------------ Fim

*******************************************************
Abrir tela de email:

CALL FUNCTION 'WS_EXECUTE'
  EXPORTING
    commandline = ' '             " Aufrufparameter
    inform      = ' '             " Warten auf Programmende
    program     = 'mailto:teste@teste.com?subject=Assunto&body=Corpo'
  EXCEPTIONS
    OTHERS      = 17.

WRITE: 'sy-subrc =', sy-subrc.



*******************************************************
Detalhes técnicos de uma tabela:

CALL FUNCTION 'VIEW_GET_DDIC_INFO'

*******************************************************
PARAMETERS dbtab TYPE c LENGTH 30. 

SELECT-OPTIONS columns FOR dbtab NO INTERVALS. 

DATA: otab  TYPE abap_sortorder_tab, 
      oline TYPE abap_sortorder, 
      dref  TYPE REF TO data. 

FIELD-SYMBOLS: <column> LIKE LINE OF columns, 
               <itab> TYPE STANDARD TABLE. 

TRY. 
    CREATE DATA dref TYPE STANDARD TABLE OF (dbtab). 
    ASSIGN dref->* TO <itab>. 
  CATCH cx_sy_create_data_error. 
    MESSAGE 'Wrong data type!' TYPE 'I' DISPLAY LIKE 'E'. 
    LEAVE PROGRAM. 
ENDTRY. 

TRY. 
    SELECT * 
           FROM (dbtab) 
           INTO TABLE <itab>. 
  CATCH cx_sy_dynamic_osql_semantics. 
    MESSAGE 'Wrong database table!' TYPE 'I' DISPLAY LIKE 'E'. 
    LEAVE PROGRAM. 
ENDTRY. 

LOOP AT columns ASSIGNING <column>. 
  oline-name = <column>-low. 
  APPEND oline TO otab. 
ENDLOOP. 

TRY. 
    SORT <itab> BY (otab). 
  CATCH cx_sy_dyn_table_ill_comp_val. 
    MESSAGE 'Wrong column name!' TYPE 'I' DISPLAY LIKE 'E'. 
    LEAVE PROGRAM. 
ENDTRY. 


*******************************************************
Verificar se usuário tem acesso à uma transação:
  call function 'AUTHORITY_CHECK_TCODE'
    exporting
      tcode  = i_tcode
    exceptions
      ok     = 0
      not_ok = 2
      others = 3.

*******************************************************

authority-check object 'S_TABU_DIS'
(12:03 PM) Luismar Antônio Correia Borges: objeto que permite o usuário a ler tabelas
(12:04 PM) Luismar Antônio Correia Borges: Atividade '03'.

*******************************************************
authority-check object 'ZATUALIZA'
       ID 'BUKRS' FIELD pv_bukrs
       id 'ACTVT' field '02'.   "Modificar

if sy-subrc <> 0.
  message s368(00) with 'Usuário sem permissão para executar o                          programa!'.
endif.

-----------------------------------------------
O 'DUMMY' libera acesso a campo, atividade...
    AUTHORITY-CHECK OBJECT 'V_KONH_VKO'
             ID 'VKORG' DUMMY
             ID 'VTWEG' FIELD wa_zsdt004-vtweg
             ID 'SPART' DUMMY
             ID 'ACTVT' DUMMY.
----
  authority-check object 'S_DEVELOP'
   id 'DEVCLASS' dummy
   id 'OBJTYPE'  field 'DEBUG'
   id 'OBJNAME'  dummy
   id 'P_GROUP'  dummy
   id 'ACTVT'    field '03'.

----------------------------------------
Tabela  (AGR_1251), "objeto autorização
                                   AGR_NAME   = Perfil 
                                   OBJECT         = Objeto de Autorização utilizado
                                    FIELD            = Campo de validação
                                    LOW e HIGH  = São os valores que você necessita

AGR_USERS (Atribuição funções a usuários)

Função 'susr_user_parameters_get' (busca parâmetros do usuário)

  data: lt_param type ustyp_t_parameters,
        lw_param type ustyp_parameters.

  call function 'susr_user_parameters_get'
    exporting
      user_name                 = p_user   "(usuário)
    tables
      user_parameters           = lt_param
   exceptions
     user_name_not_exist        = 1
     others                     = 2 .

*******************************************************
Função executada quando iniciamos qualquer transação no OKCODE

SAPLSFW_COMMON                 / LSFW_COMMONU01
FUNCTION                       / SFW_GET_SWITCHPOS

*******************************************************
Transação AL11 (Diretórios SAP)
*******************************************************
DATA: obj_type       LIKE bapiache02-obj_type,
      obj_key       LIKE bapiache02-obj_key,
      obj_sys       LIKE bapiache02-obj_sys,
      documentheader LIKE bapiache08, 
      accountgl      LIKE bapiacgl08 OCCURS 0 WITH HEADER LINE,
      currencyamount LIKE bapiaccr08 OCCURS 0 WITH HEADER LINE,
      return         LIKE bapiret2 OCCURS 0 WITH HEADER LINE,
      extension1     LIKE bapiextc OCCURS 0 WITH HEADER LINE,
      t_edidd        LIKE edidd OCCURS 0 WITH HEADER LINE,
      bapi_retn_info LIKE bapiret2 OCCURS 0 WITH HEADER LINE.
 
DATA: error_flag.
 
documentheader-username = sy-uname.
documentheader-header_txt = 'Teste BAPI'.
documentheader-comp_code = '1000'.
documentheader-doc_date = sy-datum.
documentheader-pstng_date = sy-datum.
documentheader-doc_type = 'SA'.
 
accountgl-itemno_acc = '1'.
accountgl-gl_account = '0000160100'.
accountgl-comp_code = '1000'.
accountgl-pstng_date = sy-datum.
accountgl-doc_type = 'SA'.
accountgl-profit_ctr = '0000010000'.
APPEND accountgl.
 
accountgl-itemno_acc = '2'.
accountgl-gl_account = '0000160100'.
accountgl-comp_code = '1000'.
accountgl-pstng_date = sy-datum.
accountgl-doc_type = 'SA'.
accountgl-profit_ctr = '0000010000'.
APPEND accountgl.
 
currencyamount-itemno_acc = '1'.
currencyamount-currency = 'BRL'.
currencyamount-amt_doccur = '100.00'.
APPEND currencyamount.
 
currencyamount-itemno_acc = '2'.
currencyamount-currency = 'BRL'.
currencyamount-amt_doccur = '-100.00'.
APPEND currencyamount.
 
CALL FUNCTION 'BAPI_ACC_GL_POSTING_POST'
  EXPORTING
    documentheader = documentheader
* importing
*    obj_type        = obj_type
*    obj_key         = obj_key
*    obj_sys         = obj_sys
  TABLES
    accountgl       = accountgl
    currencyamount  = currencyamount
    return          = return
    extension1      = extension1
  EXCEPTIONS
    OTHERS = 1.
IF sy-subrc  0.
  MESSAGE e000(oo) WITH 'Erro ao executar BAPI!'.
ELSE.
  LOOP AT return.
    IF NOT return IS INITIAL.
      CLEAR bapi_retn_info.
      MOVE-CORRESPONDING return TO bapi_retn_info.
      IF return-type = 'A' OR return-type = 'E'.
        error_flag = 'X'.
      ENDIF.
      APPEND bapi_retn_info.
    ENDIF.
  ENDLOOP.
  IF error_flag = 'X'.
    MESSAGE e000(oo) WITH 'Erro ao executar BAPI!'.
    ROLLBACK WORK.
  ELSE.
    COMMIT WORK AND WAIT.
  ENDIF.
ENDIF.

*******************************************************
 

  DATA: e_j1bnfdoc      LIKE bapi_j_1bnfdoc,
        e_docnum        LIKE bapi_j_1bnfdoc-docnum,
        i_j1bnflin      LIKE bapi_j_1bnflin OCCURS 0 WITH HEADER LINE,
        i_j1bnflin_add  LIKE bapi_j_1bnflin_add
                                            OCCURS 0 WITH HEADER LINE,
        i_j1bnfstx      LIKE bapi_j_1bnfstx OCCURS 0 WITH HEADER LINE,
        i_j1bnfftx      LIKE bapi_j_1bnfftx OCCURS 0 WITH HEADER LINE,
        i_bapiret2      LIKE bapiret2       OCCURS 0 WITH HEADER LINE.

* OBJ_HEADER

        e_j1bnfdoc-nftype = à categoria de nota.
        e_j1bnfdoc-doctyp = '1'.
        e_j1bnfdoc-direct = '1'.
        e_j1bnfdoc-series = '2'.

        e_j1bnfdoc-docdat = sy-datum.
        e_j1bnfdoc-pstdat = sy-datum.
        e_j1bnfdoc-credat = sy-datum.
        e_j1bnfdoc-cretim = sy-uzeit.
        e_j1bnfdoc-crenam = sy-uname.
        e_j1bnfdoc-form   = à formulário NF.
        e_j1bnfdoc-model  = '01'.
        e_j1bnfdoc-entrad = 'X'.
        e_j1bnfdoc-manual = 'X'.
        e_j1bnfdoc-waerk  = 'BRL'.
        e_j1bnfdoc-belnr  = à documento contábil.
        e_j1bnfdoc-gjahr  = sy-datum(4).
        e_j1bnfdoc-bukrs  = à empresa.
        e_j1bnfdoc-branch = '0001'.
        e_j1bnfdoc-parvw  = 'LF'.
        e_j1bnfdoc-parid  = à parceiro. 
        e_j1bnfdoc-partyp = 'V'.
        e_j1bnfdoc-inco1  = 'FOB'.
        e_j1bnfdoc-inco2  = 'FOB'.

  e_j1bnfdoc-observat = 'REGIME ESPECIAL'

* OBJ_ITEM
      i_j1bnflin-itmnum     = '10'.
      i_j1bnflin-matkl      = '500'.

      i_j1bnflin-matnr      = '000000000000000090'.
      i_j1bnflin-maktx      = 'Produtos Diversos'.
      i_j1bnflin-nbm        = '08051000'.
      i_j1bnflin-matorg     = '0'.
      i_j1bnflin-taxsit     = '00'.
      i_j1bnflin-taxsi2     = '1001'.
      i_j1bnflin-ownpro     = 'X'.
      i_j1bnflin-matuse     = '1'.
      i_j1bnflin-meins      = 'CX'.
      i_j1bnflin-taxlw1     = 'IC0'.
      i_j1bnflin-taxlw2     = 'IP1'.
      i_j1bnflin-itmtyp     = '01'.
      i_j1bnflin-incltx     = 'X'.
      i_j1bnflin-cfop_10    = '2101AA'.
      APPEND i_j1bnflin.

* OBJ_ITEM_ADD
      i_j1bnflin_add-itmnum = '10'.
      APPEND i_j1bnflin_add.

* OBJ_ITEM_TAX
      i_j1bnfstx-itmnum  = '10'.
      i_j1bnfstx-taxtyp  = 'ICM1'.
      i_j1bnfstx-taxval  = 12.
      APPEND i_j1bnfstx.
      CLEAR i_j1bnfstx.
      i_j1bnfstx-itmnum  = '10'.
      i_j1bnfstx-taxtyp  = 'IPI2'.
      APPEND i_j1bnfstx.
      CLEAR i_j1bnfstx.

* OBJ_HEADER_MSG
        i_j1bnfftx-seqnum  = '02'.
        i_j1bnfftx-linnum  = '01'.
        i_j1bnfftx-message = 'Mensagem. . .1'.
        APPEND i_j1bnfftx.
        CLEAR i_j1bnfftx.
        i_j1bnfftx-seqnum  = '01'.
        i_j1bnfftx-linnum  = '01'.
        i_j1bnfftx-message = 'Mensagem. . .1.1'.
        APPEND i_j1bnfftx.
        CLEAR i_j1bnfftx.
        i_j1bnfftx-seqnum  = '01'.
        i_j1bnfftx-linnum  = '02'.
        i_j1bnfftx-message = 'Mensagem. . .2'.
        APPEND i_j1bnfftx.
        CLEAR i_j1bnfftx.
        i_j1bnfftx-seqnum  = '01'.
        i_j1bnfftx-linnum  = '03'.
        i_j1bnfftx-message = 'Mensagem. . .3'.
        APPEND i_j1bnfftx.
        CLEAR i_j1bnfftx.
        i_j1bnfftx-seqnum  = '01'.
        i_j1bnfftx-linnum  = '04'.
        i_j1bnfftx-message = = 'Mensagem. . .4'.
        APPEND i_j1bnfftx.
        CLEAR i_j1bnfftx.
      ENDIF.


      CALL FUNCTION 'BAPI_J_1B_NF_CREATEFROMDATA'
        EXPORTING
          obj_header     = e_j1bnfdoc
        IMPORTING
          e_docnum       = e_docnum
        TABLES
          obj_item       = i_j1bnflin
          obj_item_add   = i_j1bnflin_add
          obj_item_tax   = i_j1bnfstx
          obj_header_msg = i_j1bnfftx
          return         = i_bapiret2.

*******************************************************
REPORT  ZTES_XML.
data g_ixml type ref to IF_IXML.
data: g_document type ref to  IF_IXML_DOCUMENT,
      lote type ref to IF_IXML_ELEMENT,
      nfnum type ref to IF_IXML_ELEMENT.


g_ixml = cl_ixml=>create( ).

g_document = g_ixml->create_document( ).

lote = g_document->create_simple_element(
                   name = 'NF'
                   parent = g_document ).

nfnum = g_document->create_simple_element(
                               name  = 'nfnum'
                               value = '999999999'
                               parent = lote ).

g_document->create_simple_element(
                               name  = 'rafael'
                               value = '999999999'
                               parent = nfnum ).

g_document->create_simple_element(
                               name  = 'nxnum'
                               value = '999999999'
                               parent = lote ).

*g_document->create_simple_element(
*                   name = 'NF2'
*                   value = '999999999'
*                   parent = g_document ).

data: g_streamfactory TYPE REF TO IF_IXML_STREAM_FACTORY,
      g_encoding type ref to IF_IXML_ENCODING ,
      g_ostream TYPE REF TO IF_IXML_ostream,
      g_renderer type REF TO IF_IXML_renderer.
*   Creating a stream factory and streams
g_streamfactory = g_ixml->create_stream_factory( ).

g_encoding = g_ixml->create_encoding(
                      character_set = 'utf-8'
                      byte_order = if_ixml_encoding=>co_little_endian ).


types: t_spool_xsf_line(255) type x.
data: xml_table    type table of t_spool_xsf_line,
      xml_size   type i.

*      BEGIN OF XML_TABLE OCCURS 0,
*    line like LINE OF table,
*  END OF xml_table.

data: xxml type xstring.
data cmxl  type string.


* Rendering into a table-based stream
g_ostream = g_streamfactory->create_ostream_itable( xml_table ).
*g_ostream = g_streamfactory->CREATE_OSTREAM_CSTRING( Cmxl ).

g_ostream->set_encoding( encoding = g_encoding ).

g_renderer = g_ixml->create_renderer( ostream  = g_ostream
                                      document = g_document ).
data g_rc type i.
g_rc = g_renderer->render( ).

* use only the first xml_size bytes of the xml table!!
xml_size = g_ostream->get_num_written_raw( ).

data: v_file type string.

concatenate 'C:\' 'FUL05' '.xml' into v_file.

call method cl_gui_frontend_services=>gui_download
  EXPORTING
    bin_filesize = xml_size
    filename     = v_file
    filetype     = 'BIN'
  CHANGING
    data_tab     = xml_table. 

*******************************************************
form obtem_caracteristica using p_atinn " like ausp-atinn
                                p_kunnr like kna1-kunnr
                         changing p_atflv.

constants: c_klart             like ausp-klart  value '011'.

  data: l_objec  like ausp-objek,
        v_atinn  type ausp-atinn,
        t_ausp   type table of ausp.

  clear: t_ausp[], t_ausp.
  move p_kunnr to l_objec.

  call function 'CONVERSION_EXIT_ATINN_INPUT'
    exporting
      input  = p_atinn   "Por exemplo: 'Z_SD_DATA_CRIACAO'
    importing
      output = v_atinn
    exceptions
      others = 1.

  call function 'CLSE_SELECT_AUSP'
    exporting
      klart                     = c_klart
      objek                     = l_objec
      atinn                     = v_atinn
      key_date                  = sy-datum
    tables
      t_ausp                    = t_ausp
    exceptions
      no_entry_found            = 1
      parameters_not_sufficient = 2
      others                    = 3.
  if sy-subrc <> 0.
  endif.
  read table t_ausp into wa_ausp index 1.
  if sy-subrc eq 0.
    p_atflv = wa_ausp-atflv.    "type ausp-atflv
  endif.


Outra maneira:
  select aufnr OBJNR KTEXT into table t_coas
         from coas
              where aufnr in s_aufnr
                and auart in s_auart.

  if sy-subrc ne 0.
    message i000 with text-m01. "Nenhuma ordem encontrada!
    stop.
  endif.

data: v_atinn  type ausp-atinn.
constants: c_caracteristica    type ausp-atinn  value 'ZSAP_KKR_ORPRDHA',   "Característica interna
           c_klart             type ausp-klart  value '013'.                "Tipo de classe

  "Executa função que converte a característica interna (ZSAP_KKR_ORPRDHA) p/ numérico
  call function 'CONVERSION_EXIT_ATINN_INPUT'
    exporting
      input  = c_caracteristica   "ZSAP_KKR_ORPRDHA --- Hierarquia de produtos
    importing
      output = v_atinn            "0000000873
    exceptions
      others = 1.

  if sy-subrc ne 0.
    message i000 with text-m02. "Erro conversão característica interna (ZSAP_KKR_ORPRDHA)!
    stop.
  endif.

  "Busca valores (Característica ZSAP_KKR_ORPRDHA --- Hierarquia de produtos) das ordens encontradas acima
  select objek atwrt into table t_ausp
         from ausp
           FOR ALL ENTRIES IN t_coas
              where objek = t_coas-objek
                and atinn = v_atinn.

*******************************************************
O primeiro exemplo voce usa quando voce estiver enviando os dados do seu banco dados (SAP) para o outro banco de dados (MASTERSAF).
 
    EXEC SQL.
      INSERT INTO NOME DA SUA TABELA
      ( age_codigo,
        ban_codigo,
        cnt_codigo,
        conta_contabil,
        data_caixa,
        DATA_COMPETENCIA,
        DATA_PAGAMENTO,
        DOC_ORIGEM,
        doc_pagador,
        EMITIDO_FLAG,
        HISTORICO,
        MAN_AUT,
        ORIGEM_PK,
        ORIGEM_SISTEMA,
        PFJ_CEP,
        PFJ_CIDADE,
        PFJ_CODIGO,
        PFJ_COMPLEMENTO,
        PFJ_CPF,
        PFJ_DESCRICAO,
        PFJ_ENDERECO,
        PFJ_NOME,
        PFJ_NUMERO,
        PFJ_UF,
        TDO_CODIGO,
        TDP_CODIGO,
        TEMPERATURA,
        VALOR )
     VALUES
        ( :ti_ctas_pg-AGE_CODIGO,
        :ti_ctas_pg-BAN_CODIGO,
        :ti_ctas_pg-CNT_CODIGO,
        :t_oracle-conta_contabil,
        :t_oracle-data_caixa,
        :t_oracle-DATA_COMPETENCIA,
        :t_oracle-DATA_PAGAMENTO,
        :t_oracle-DOC_ORIGEM,
        :t_oracle-doc_pagador,
        :t_oracle-EMITIDO_FLAG,
        :t_oracle-HISTORICO,
        :t_oracle-MAN_AUT,
        :t_oracle-ORIGEM_PK,
        :t_oracle-ORIGEM_SISTEMA,
        :t_oracle-PFJ_CEP,
        :t_oracle-PFJ_CIDADE,
        :t_oracle-PFJ_CODIGO,
        :t_oracle-PFJ_COMPLEMENTO,
        :t_oracle-PFJ_CPF,
        :t_oracle-PFJ_DESCRICAO,
        :t_oracle-PFJ_ENDERECO,
        :t_oracle-PFJ_NOME,
        :t_oracle-PFJ_NUMERO,
        :t_oracle-PFJ_UF,
        :t_oracle-TDO_CODIGO,
        :t_oracle-TDP_CODIGO,
        :t_oracle-TEMPERATURA,
        :t_oracle-VALOR )
    ENDEXEC.
    commit work.
  endloop.
 
E nesse segundo modelo é quando voce estiver buscando os dados em outro banco de dados (MASTERSAF) e gravando no bando de dados do (SAP).
 
* Selecionar dados direto do banco de dados oracle do XRT
    EXEC SQL PERFORMING write_registro.
      select bldat,   blart,   bukrs,   budat,  monat,
             waers,   newbs,   newko,   wrbtr,  valut,
             sgtxt,   newbs1,  newk01,  wrbtr1, zuonr1,
             sgtxt1,  numlote, parcont, status, critica,
             tsgrava, tsleitu, tsxrt
             into :NOME DA SUA TABELA-bldat,
                  :NOME DA SUA TABELA-blart,
                  :NOME DA SUA TABELA-bukrs,
                  :NOME DA SUA TABELA-budat,
                  :NOME DA SUA TABELA-monat,
                  :NOME DA SUA TABELA-waers,
                  :NOME DA SUA TABELA-newbs,
                  :NOME DA SUA TABELA-newko,
                  :NOME DA SUA TABELA-wrbtr,
                  :NOME DA SUA TABELA-valut,
                  :NOME DA SUA TABELA-sgtxt,
                  :NOME DA SUA TABELA-newbs1,
                  :NOME DA SUA TABELA-newk01,
                  :NOME DA SUA TABELA-wrbtr1,
                  :NOME DA SUA TABELA-zuonr1,
                  :NOME DA SUA TABELA-sgtxt1,
                  :NOME DA SUA TABELA-numlote,
                  :NOME DA SUA TABELA-parcont,
                  :NOME DA SUA TABELA-status,
                  :NOME DA SUA TABELA-critica,
                  :NOME DA SUA TABELA-tsgrava,
                  :NOME DA SUA TABELA-tsleitu,
                  :NOME DA SUA TABELA-tsxrt
             from NOME DA SUA TABELA
             where critica IS NULL
    ENDEXEC.

*&---------------------------------------------------------------------
*&      Form  write_registro
*&---------------------------------------------------------------------
*       text
*----------------------------------------------------------------------
FORM write_registro.
  APPEND NOME DA SUA TABELA.
ENDFORM.                    "write_registro

Para funcione a sua tabela precisa ter a mesma estrutura da tabela do outro sistema e a empresa precis criar essa conexão de DBLINK no banco de dados ligando os dois banco de dados.

*******************************************************************************************************************
Acessar DB externo (não SAP)

incluir os dados da conexão dblink na transação DBCO.
* DBFINT é o nome do DBlink 

   EXEC sql.
      CONNECT TO 'DBFINT' AS 'CON1'
   ENDEXEC.

  EXEC sql.
    SET CONNECTION 'CON1'
  ENDEXEC.

EXEC SQL PERFORMING loop_output.                           
    SELECT serial, id, conc, event_group, start_date           
    INTO :wa                                                   
    FROM nome_da_tabela@nome_do_dblink                                                                       
ENDEXEC.                                                   

EXEC sql.
    DISCONNECT 'CON1'
ENDEXEC.

Para testar conexão com o banco: programa ADBC_TEST_CONNECTION.
----------------------------------------------------------------

Pode acessar as tabelas via EXEC SQL, deve ser criado um “Sinônimo” no banco de dados DBLINK e ter o perfil de acesso.

Select com uma tabela:

DATA: BEGIN OF t_centros OCCURS   0,
        cod_estoque(5),
        werks LIKE zmmt026-werks,
      END OF t_centros.
...

  EXEC SQL PERFORMING F_CARREGA_CENTROS.
    SELECT COD_ESTOQUE,
           COD_CENTRO_SAP
      INTO :T_CENTROS
      FROM ESTOQUE
  ENDEXEC.
...

FORM f_carrega_centros.
  APPEND t_centros.
ENDFORM.                               " F_CARREGA_CENTROS

Select com Join de 3 tabelas:

DATA: BEGIN OF e_ordenes_material,
        ord_serv(8),
        seq(10),
        atividade(10),
        tp_open(1),
        qtde(4) TYPE n,
        equipe(11),
        dt_envio(10),
        cod_estoque(5),
        evento(12),
        cod_munic(3),
        cod_u_r,
        tp_os(5),
        num_circuito(20),
      END OF e_ordenes_material.
....

  EXEC SQL PERFORMING F_CARREGA_TABS.

    SELECT A.NUM_OS,
           A.SEQ,
           A.COD_ATIVIDADE,
           A.TIPO_MOVTO,
           A.QTDE,
           A.COD_VEICULO,
           A.DT_ENVIO_MACRO,
           A.COD_ESTOQUE,
           A.EVT_NUMBER,
           A.COD_MUNIC_GASTO,
           A.IND_URB_RUR,
           B.TIP_OS,
           C.CIRCUITO
      INTO :E_ORDENES_MATERIAL
    FROM ORDENES_MATERIAL A, ORDENES B, SUMCON C
   WHERE A.NUM_OS     = B.NUM_OS  AND
         C.NIS_RAD(+) = B.NIS_RAD AND
         A.IND_PROC   = '2'
  ENDEXEC.

.....

FORM f_carrega_tabs.

  IF e_ordenes_material-qtde CN '1234567890 ' OR
     e_ordenes_material-qtde IS INITIAL OR
     e_ordenes_material-qtde CO '0 '.
    e_ordenes_material-qtde = 1.
  ENDIF.

  MOVE-CORRESPONDING e_ordenes_material TO t_ordens.

  COLLECT t_ordens.

ENDFORM.                               " F_CARREGA_TABS

Para criar o Sinônimo:
create dblink REMOTE_DB as connect to remote_user identified by password; (database link created)
drop table ZTABLE; (table dropped)
create synonym ZTABLE for ZTABLE@REMOTE_DB; (synonym created)

*******************************************************************************************************************

rotina textos (read_text):

  loop at t_ztrt003.
    clear t_stxh_aux.
    t_stxh_aux-tdname = t_ztrt003-vbeln.
    collect t_stxh_aux.
  endloop.

  sort t_stxh_aux.
  delete t_stxh_aux where tdname is initial.
  delete adjacent duplicates from t_stxh_aux comparing tdname.

  check not t_stxh_aux[] is initial.

  "Busca registros (tabela cabeçalho TEXTOS) das ordens.
  "Se encontrar registro para a ordem, é porquê possui 'texto do vendedor'.
  select tdname tdtxtlines from stxh into table t_stxh
    for all entries in t_stxh_aux
     where tdobject = 'VBBK'
       and tdname   = t_stxh_aux-tdname
       and tdid     = 'Z002'
       and tdspras  = sy-langu.

  sort t_stxh by tdname.

  "Deleta registros que não contém linhas de textos
  delete t_stxh where tdtxtlines is initial.

--------------------------------------------------------------
chamar tela pra editar texto:

data: editor TYPE REF TO cl_gui_textedit.

         CALL METHOD EDITOR->GET_TEXT_AS_R3TABLE
           EXPORTING  ONLY_WHEN_MODIFIED  = 0
           IMPORTING  TABLE               = TEXT_TABLE_AUX
                      IS_MODIFIED         = MODIFIED_AUX
           EXCEPTIONS OTHERS = 1.

--------------------------------------------------------------
  "Luismar (06/10/2010) SS 226448 - Início
  "Limitar tamanho de texto Cliente (dados área de vendas)
  data: memory_id(30) value 'SAPLSTXD',
        catalog_index like sy-tabix.

  data begin of catalog occurs 50.
          include structure tcatalog.
  data end of catalog.

  data:
    begin of catalog_key,
      tdobject like thead-tdobject,
      tdname   like thead-tdname,
      tdid     like thead-tdid,
      tdspras  like thead-tdspras,
    end of catalog_key.

  data: v_tdid      type tdid,
        v_tdspras   type spras,
        v_tdname    type tdobname,
        v_tdobject  type tdobject,
        v_valor     type zsdt001-valor,
        v_bytes     type i.

  data: t_tline type standard table of tline with header line.

  "Prepara variáveis de textos (dados área de vendas)
  v_tdid     = 'Z001'.
  v_tdspras  = sy-langu.
  concatenate i_knvv-kunnr i_knvv-vkorg i_knvv-vtweg i_knvv-spart into v_tdname.
  v_tdobject = 'KNVV'.

  check sy-uname = 'LBORGES'.

  break lborges.

  clear   t_tline[].
  refresh t_tline[].

  "Importa da memória objeto de definição de textos...
  refresh catalog.
  import catalog from memory id 'SAPLSTXD'.

  catalog_key-tdobject = v_tdobject.
  catalog_key-tdname   = v_tdname.
  catalog_key-tdid     = v_tdid.
  catalog_key-tdspras  = sy-langu.

  clear catalog.
  read table catalog with key catalog_key.

  if catalog-function = 'I'           "Se criando novo texto...
       or catalog-function = 'U'.     "se modificando texto...
    catalog_index = sy-tabix.
    memory_id+8(6) = catalog-id.

    "Importa da memória, texto digitado para cliente...
    import tline to t_tline
          from memory id memory_id.

  endif.

  if not t_tline[] is initial.   "se encontrou textos para o cliente em 'dados área de vendas'...
    "Busca quantidades máximas atribuídas a textos...
    clear v_valor.
    select single valor into v_valor
       from zsdt001
           where processo = 'TEXTOS_CLIENTE'
             and programa = 'KNVV'
             and tipo_reg = 'Z001'.

    if ( sy-subrc = 0 ) and ( not v_valor is initial ).
      clear v_bytes.
      "Verifica a quantidade de caracteres inseridos no texto...
      loop at t_tline. " into e_tline.
        v_bytes = v_bytes + strlen( t_tline-tdline ).
      endloop.

      if v_bytes > v_valor.
        "Se qtde de caracteres digitados ultrapassar o limite permitido... mensagem de erro.
        message e000(zsd) with  'Texto da área de vendas' 'excedeu o tamanho máximo permitido: ' v_valor.
      endif.

    endif.
  endif.
  

******************************************************************************************************************* 
função ENQUEUE_READ, só com o mandante preenchido (já vem default)
> > retorna todos os objetos bloqueados no ambiente.
******************************************************************************************************************* 

ALV OO para ALV GRID

DATA: "ALV OO
      t_field         TYPE TABLE OF lvc_s_fcat WITH HEADER LINE,
      t_fieldcat      TYPE TABLE OF lvc_s_fcat,
      "
      t_fieldcat_alv  type slis_t_fieldcat_alv,
      wa_fieldcat_alv like line of t_fieldcat_alv.

  loop at t_fieldcat into t_field.

    move-corresponding t_field to wa_fieldcat_alv.
    wa_fieldcat_alv-reptext_ddic  = t_field-coltext.
    wa_fieldcat_alv-seltext_l     = t_field-scrtext_l.
    wa_fieldcat_alv-seltext_m     = t_field-scrtext_m.
    wa_fieldcat_alv-seltext_s     = t_field-scrtext_s.

    append wa_fieldcat_alv to t_fieldcat_alv.

  endloop.

  call function 'REUSE_ALV_GRID_DISPLAY'
    exporting
      i_save                  = 'A'
      i_callback_program      = sy-repid
      it_fieldcat             = t_fieldcat_alv[]
      it_sort                 = t_sort
      is_variant              = v_variant
      is_layout               = w_layout
    tables
      t_outtab                = t_relat.

ALV GRID p/ ALV OO
  loop at it_fieldcat into wa_fieldcat.
    move-corresponding wa_fieldcat to wa_fieldcat_obj.
    wa_fieldcat_obj-coltext    = wa_fieldcat-reptext_ddic.
    wa_fieldcat_obj-scrtext_l  = wa_fieldcat-seltext_l  .
    wa_fieldcat_obj-scrtext_m  = wa_fieldcat-seltext_m  .
    wa_fieldcat_obj-scrtext_s  = wa_fieldcat-seltext_s  .

    append wa_fieldcat_obj to it_fieldcat_obj.
  endloop.

  call screen 100.

******************************************************************************************************************* 

  DATA: vl_calc  TYPE i,
        vl_space TYPE string.

  SHIFT p_campo LEFT DELETING LEADING space. " Tira espaço em branco a esquerda
  vl_calc = ( wa_003-zedi_posfim_fd - wa_003-zedi_posini_fd ) - STRLEN( p_campo ).
  "Retirar sujeira de um espaço em branco.
  vl_space = cl_abap_conv_in_ce=>uccp( '00a0' ).
  DO vl_calc TIMES.
    CONCATENATE p_campo vl_space INTO p_campo. " ATENÇÃO: o espaço é um caracter em branco
  ENDDO.

******************************************************************************************************************* 
SD_SCD_ITEM_PRICING_COMPLETE

Criar um enhancement no início da função (com um 'exit') para não redeterminar preços.

******************************************************************************************************************* 
types: begin of ty_makt,
         matnr        like makt-matnr,
         maktx        like makt-maktx,
       end of ty_makt.

data:  t_makt          type table of ty_makt,
       wa_makt         type ty_makt.

   t_mbewh_aux[] = t_mbewh[].
   delete ADJACENT DUPLICATES FROM t_mbewh_aux comparing matnr.

   select matnr matktx into table t_makt
          from makt
               FOR ALL ENTRIES IN T_MBEWH_aux
          where matnr = T_MBEWH_aux-matnr
            and spras = sy-langu.

   sort t_makt by matnr.

   "Texto do material
   read table t_makt into wa_makt with key matnr = T_MBEWH-matnr binary search.

******************************************************************************************************************* 

ABAP Número Paginas no Relatório (WRITE).

O código abaixo mostra como exibir o número total de páginas em um relatório como "Página 1 de 8." 
Entretanto, isso não funciona se você executar o programa em background. 

* Declare uma variável 
DATA L_PAGE_COUNT(5) TYPE C. 

* Copie O código abaixo para o final de toda impressão dos dados do seu programa;

Contagem de página serão impressas em cada página aqui:

WRITE SY-PAGNO TO L_PAGE_COUNT LEFT-JUSTIFIED. 
DO SY-PAGNO TIMES.
  READ LINE 1 OF PAGE SY-INDEX. 
  REPLACE '#####' WITH L_PAGE_COUNT INTO SY-LISEL.
  MODIFY CURRENT LINE.
ENDDO.

TOP-OF-PAGE.
WRITE: /(70) 'Página' CENTERED, 70 SY-PAGNO,'de ', '#####'.

******************************************************************************************************************* 
Utilize a função ME_READ_HISTORY para a leitura do que ocorreu para cada item
do documento de compra.

******************************************************************************************************************* 
Determina série NF

CALL FUNCTION 'J_1BNF_GET_PRINT_CONF' 
      EXPORTING                                       
        headerdata = wa_objheader
      IMPORTING                                       
        print_conf = print_conf
      EXCEPTIONS                                      
        error      = 1       

 IF SY-SUBRC = 0.
         MOVE print_conf-SERIES TO wa_objheader-series.
         MOVE print_conf-SUBSER TO wa_objheader-subser.
         MOVE print_conf-FATURA TO wa_objheader-fatura.
 endif.
******************************************************************************************************************* 

  "Através do centro, buscar a empresa
  select single k~bukrs into l_bukrs
          from t001w as w
               inner join t001k as k
                  on w~bwkey = k~bwkey
                     where w~werks = i_werks.

  check sy-subrc = 0.
  "Luismar (08/02/2009) --- BYT-778593 - Fim

*>HYPM - MFIUZA - (1456) - 15.12.2009 - Inicio
  "Busca série NF da atribuição BRANCH e VSTEL
  select single series
         into l_series
              from j_1bb2dynamic
                   where bukrs = l_bukrs and  "Luismar (08/02/2009) BYT-778593
                        branch = i_werks and
                          form = 'NF01'  and
                         value = i_vstel.

  if sy-subrc ne 0.
    select single series
       into l_series
            from j_1bb2dynamic
                 where bukrs = l_bukrs and
                      branch = i_werks and
                        form = 'NF55'  and
                       value = i_vstel.

    if sy-subrc ne 0.
      "Se não encontrar série por VSTEL (local de expedição), busca por atribuição BRANCH
      select single series
             into l_series
                  from j_1bb2
                       where bukrs = l_bukrs and "Luismar (08/02/2009) BYT-778593
                            branch = i_werks and
                              form = 'NF01'.

      if sy-subrc ne 0.
        select single series
               into l_series
                    from j_1bb2
                         where bukrs = l_bukrs and
                              branch = i_werks and
                                form = 'NF55'.
      endif.
    endif.
  endif.

******************************************************************************************************************* 
Tabela com descrição de dumps
SNAP_BEG (ST22) DUMPS

  SELECT * FROM snap_beg INTO wa_snap_beg
            WHERE seqno = '000'
            AND   datum IN s_datum
            AND   uzeit IN s_uzeit
            AND   ahost IN s_ahost
            AND   modno IN s_wpid
            AND   uname IN s_uname
            AND   mandt IN s_mandt
            AND   xhold IN s_xhold
            ORDER BY datum DESCENDING uzeit DESCENDING ahost uname mandt modno.

----------------------
DUMP --- é um erro na interpretação de código abap.
         Também podem ocorrer dumps que estão relacionados ao ambiente...
         por exemplo quando expira o tempo de execução de um programa... ocorre um dump 'time out' (tempo de execução excedido),
         não interpretação de codificação. É uma dependência de configurações para execucões de processos.
         Atualmente, está configurado, para 10 minutos, o tempo de execução de um processo.
         Outro dump (que na maioria das vezes não depende da codificação de um programa):
         Quando o usuário executa um relatório, onde este, pelo fato do usuário ter escolhido
         um grande período de pesquisa, ultrapassa o espaço reservado para armazenamento de processos na memória.

******************************************************************************************************************* 
BP_JOB_DELETE
TBTCO (tabela jobs) - Tabela de síntese de status de job
**********************************************************************************************Perform usando parameter tables (parâmetro tabelas)
PROGRAM FORM_TEST.

DATA: BEGIN OF LINE,
COL1 TYPE I,
COL2 TYPE I,
END OF LINE.

DATA ITAB LIKE STANDARD TABLE OF LINE.

PERFORM FILL CHANGING ITAB.

PERFORM OUT USING ITAB.

FORM FILL CHANGING F_ITAB LIKE ITAB.

  DATA F_LINE LIKE LINE OF F_ITAB.

  DO 3 TIMES.
    F_LINE-COL1 = SY-INDEX.
    F_LINE-COL2 = SY-INDEX ** 2.
    APPEND F_LINE TO F_ITAB.
  ENDDO.

ENDFORM.

FORM OUT USING VALUE(F_ITAB) LIKE ITAB.

  DATA F_LINE LIKE LINE OF F_ITAB.

  LOOP AT F_ITAB INTO F_LINE.
    WRITE: / F_LINE-COL1, F_LINE-COL2.
  ENDLOOP.

ENDFORM.

***************************************************************************************
Buscar modificações em tabelas
Tabela SE16N_CD_KEY
***************************************************************************************
Mensagens em popup:

data:  BEGIN OF type_log_estorno,
            msgty TYPE smesg-msgty,
            msgv1 TYPE char100,
            msgv2 TYPE char100,
            msgv3 TYPE char100,
            msgv4 TYPE char100,
            zeile TYPE char3,
        END OF type_log_estorno.

data: t_log_estorno    TYPE TABLE OF type_log_estorno   WITH HEADER LINE.

----
        clear t_log_estorno.

        add 1 to v_zeile.

        move 'nota cancelada somente aceita ação 5' to t_log_estorno-msgv4.
        concatenate 'Série'
                    t_ztrt003-serie
               into t_log_estorno-msgv3 separated by space.

        move: 'E'             to t_log_estorno-msgty,
              'NF/NFe:'       to t_log_estorno-msgv1,
              t_ztrt003-numnf to t_log_estorno-msgv2,
              v_zeile         to t_log_estorno-zeile.

        append t_log_estorno.


  if t_log_estorno[] is not initial.

    call function 'MESSAGES_INITIALIZE'
      exceptions
        log_not_active       = 1
        wrong_identification = 2
        others               = 3.

    loop at t_log_estorno.

      call function 'MESSAGE_STORE'
        exporting
          arbgb                  = 'ZTR'
          msgty                  = t_log_estorno-msgty
          msgv1                  = t_log_estorno-msgv1
          msgv2                  = t_log_estorno-msgv2
          msgv3                  = t_log_estorno-msgv3
          msgv4                  = t_log_estorno-msgv4
          txtnr                  = '000'
          zeile                  = t_log_estorno-zeile
        exceptions
          message_type_not_valid = 1
          not_active             = 2
          others                 = 3.

    endloop.

    call function 'MESSAGES_SHOW'
      exporting
        i_use_grid         = 'X'
      exceptions
        inconsistent_range = 1
        no_messages        = 2
        others             = 3.

    call function 'MESSAGES_STOP'
      exceptions
        a_message         = 1
        e_message         = 2
        w_message         = 3
        i_message         = 4
        s_message         = 5
        deactivated_by_md = 6
        others            = 7.

  endif.

***************************************************************************************
Quebrar linha arquivo texto (txt) ----- tirar espaços final linha

gs_arq-linha+98(2) = cl_abap_char_utilities=>cr_lf. " Quebra linha

Ou a função abaixo (usa a classe cl_abap_char_utilities=>cr_lf):
DATA:
   dta_cr(1)        TYPE c,  "Carriage Return
   dta_lf(1)        TYPE c,  "Line Feed (avanço linha)
   dta_crlf(2)      TYPE c.    "carriage return + line feed
  CALL FUNCTION 'FI_DME_CHARACTERS'
    IMPORTING
      e_cr   = dta_cr
      e_lf   = dta_lf
      e_crlf = dta_crlf.

mova a variavel dta_ctrl para as 2 ultimas posições da linha do arquivo
linha_arquivo+xxx(2) = dta_ctrl.


------------------------------
EX: 
  DATA: v_cr_lf TYPE char02.
  v_cr_lf = cl_abap_char_utilities=>cr_lf. 

Ou 

*(for Unicode enabled abap)
Constants: c_hex13 type x value '13'.
field-symbols: <fs> type any.
Data: string_13 type c.
Assign string_13 to <fs> casting type x.
<fs> = c_hex13. 
Constants: c_hex10 type x value '10'.

***************************************************************************************
Funções objetos de intervalo (snro), bloqueio.

FORM NUMERAR_DOCTO.
  DATA: LC_CONTROLE LIKE ZARM002A-ID.
* Bloqueia o objeto
  CALL FUNCTION 'NUMBER_RANGE_ENQUEUE'
       EXPORTING
            OBJECT           = 'ZARM002A'
       EXCEPTIONS
            FOREIGN_LOCK     = 1
            OBJECT_NOT_FOUND = 2
            SYSTEM_FAILURE   = 3
            OTHERS           = 4.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      NR_RANGE_NR                   = '01'
      OBJECT                        = 'ZARM002A'
      QUANTITY                      = '00000000000000000001'
*   SUBOBJECT                     = ' '
*   TOYEAR                        = '0000'
*   IGNORE_BUFFER                 = ' '
    IMPORTING
      NUMBER                        = LC_CONTROLE
*   QUANTITY                      =
*    RETURNCODE                    =
    EXCEPTIONS
      INTERVAL_NOT_FOUND            = 1
      NUMBER_RANGE_NOT_INTERN       = 2
      OBJECT_NOT_FOUND              = 3
      QUANTITY_IS_0                 = 4
      QUANTITY_IS_NOT_1             = 5
      INTERVAL_OVERFLOW             = 6
      BUFFER_OVERFLOW               = 7
      OTHERS                        = 8.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

* Desbloqueia o objeto de numeração
  CALL FUNCTION 'NUMBER_RANGE_DEQUEUE'
       EXPORTING
            OBJECT           = 'ZARM002A'
       EXCEPTIONS
            OBJECT_NOT_FOUND = 1
            OTHERS           = 2.
  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  ZARM002A-ID = LC_CONTROLE.
ENDFORM.                    " numerar_docto

***************************************************************************************
-----Quantidades (estoque) em trânsito.
  call function 'MB_ADD_TRANSFER_QUANTITY'
    exporting
      cumulate            = ''
      i_cross_company     = 'X'
      i_non_cross_company = 'X'
    tables
      xwerks              = r_werks
      xtab6               = t_trans
    exceptions
      others              = 1.

Ou...
Se contemplar data pedido compra:
  types: begin of ty_s_mdbs,
          matnr like ekpo-matnr,
          ebeln like ekpo-ebeln,
          ebelp like ekpo-ebelp,
          pstyp like ekpo-pstyp,
          umren like ekpo-umren,
          umrez like ekpo-umrez,
          werks like ekpo-werks,
          menge like eket-menge,
          wamng like eket-wamng,
          wemng like eket-wemng,
          bstmg like ekpo-menge,
          retpo like ekpo-retpo,
        end of ty_s_mdbs.

  data: xmdbs type standard table of ty_s_mdbs with header line.

  "Busca estoque que está em trânsito (pesquisa em doctos materiais)
  select matnr ekpo~ebeln ekpo~ebelp pstyp umren umrez werks
       eket~menge wamng wemng ekpo~menge as bstmg retpo
       into table xmdbs
       from ( ekko inner join ekpo
              on  ekko~mandt = ekpo~mandt
              and ekko~ebeln = ekpo~ebeln
         )
            inner join eket
            on  ekpo~mandt = eket~mandt
            and ekpo~ebeln = eket~ebeln
            and ekpo~ebelp = eket~ebelp
      for all entries in t_mara
       where matnr      eq t_mara-matnr
         and werks      eq p_werks
         and ekko~bedat le v_data
         and reswk      ne space       "centro de saída estoque preenchido...
         and elikz = space             "código de remessa final vazio...
         and ekpo~loekz eq space       "somente material não deletado...
         and ekpo~bstyp in ('F', 'L')  "categoria do documento de compras 'F' ou 'L'...
         and eket~wamng ne eket~wemng  "'Qtd.saída' diferente de 'Quantidade de entrada'...
         and ekpo~stapo ne 'X'.        "Item não é estatístico.

  check sy-subrc = 0.

  "Excluir se não houver estoque clássico em trânsito (PSTYP 0,3,7).
  delete xmdbs where pstyp na '037'.

  "Excluir os registros com quantidade zerada
  delete xmdbs where menge is initial.

  check not xmdbs[] is initial.

  refresh t_transito.
  loop at xmdbs.

    clear wa_transito.
    wa_transito-matnr = xmdbs-matnr.
    wa_transito-werks = p_werks.

    data: l_bstmg like ekpo-menge.

    "Qtd.saída - Quantidade de entrada
    l_bstmg = xmdbs-wamng - xmdbs-wemng.

    if not xmdbs-retpo is initial. "Se for item de devolução...
      l_bstmg = 0 - l_bstmg.       "troca o sinal (- p/ +)
    endif.
    wa_transito-saldo = l_bstmg * xmdbs-umrez / xmdbs-umren.

    collect wa_transito into t_transito.

  endloop.

  sort t_transito by werks matnr.
***************************************************************************************
ME9A                 Saída de mensagens solicitações cot.
ME9E                 Saída de mensagens divisões prg.rem.
ME9F                 Saída de mensagens pedidos
ME9K                 Saída de mensagens contratos
ME9L                 Saída de mensagens programas remessa
***************************************************************************************
Lista técnica materiais: Função CS_BOM_EXPL_MAT_V2

***************************************************************************************
Simula custo de frete de transporte:

CALL FUNCTION 'SD_SCD_SIMULATE_FREIGHT_COSTS'
  EXPORTING
    i_shipments             = l_simulation-shp-shipments
    i_scd_sim               = g_opt_sim
    i_prsdt                 = i_opt_prsdt
  IMPORTING
    e_freight_costs         = l_simulation-scd-freight_costs
    e_errors_occured        = l_simulation-scd-errors_occured
    e_warnings_occured      = l_simulation-scd-warnings_occured
    e_created_freight_costs = l_simulation-scd-count
  TABLES
    c_log_file              = l_simulation-scd-log_file
  EXCEPTIONS
    OTHERS                  = 0.

***************************************************************************************
  Verificar se diretório existe:
      WHEN p_local.

        CALL METHOD cl_gui_frontend_services=>(l_directory_exist)
          EXPORTING
            directory    = v_diretorio
          RECEIVING
            result       = l_boolean
          EXCEPTIONS
            cntl_error   = 2
            error_no_gui = 2.
        IF sy-subrc EQ 0.
          IF l_boolean NE c_x.
            MESSAGE e002(strj).
          ENDIF.
        ENDIF.

*     Verificar diretório no servidor
      WHEN p_serv.

        l_path = v_diretorio.
        CALL FUNCTION 'SUBST_GET_FILE_LIST'
          EXPORTING
            dirname      = l_path
            filenm       = ''
          TABLES
            file_list    = t_file_list
          EXCEPTIONS
            access_error = 1
            OTHERS       = 2.
        IF sy-subrc NE 0.
          MESSAGE e002(strj).
        ENDIF.
        DELETE t_file_list WHERE type NE 'directory'.
    ENDCASE.

--------------------------
Verificar se diretório existe:
data: vl_caminho       type DSVASDOCID.
      vl_diretorio(50) type c.
  data: it_file_table type table of sdokpath,
        it_dir_table  type table of sdokpath.

  call function 'TMP_GUI_DIRECTORY_LIST_FILES'
    exporting
      directory  = vl_caminho
      filter     = vl_diretorio
    tables
      file_table = it_file_table
      dir_table  = it_dir_table
    exceptions
      cntl_error = 1
      others     = 2.

  if not it_dir_table[] is initial.
    vl_existe = c_yes.
  else.

    "Criar diretorio...
    call function 'TMP_GUI_CREATE_DIRECTORY'
      exporting
        dirname = vl_caminho
      exceptions
        failed  = 1
        others  = 2.
    if sy-subrc is initial.
      move c_yes to vl_existe.
    endif.
  endif.

------------------------------------
    " Copia arquivo para outra pasta e apaga da pasta original
    call function 'ISH_DRGCDF_FILE_COPY'
      exporting
        ss_source_filename      = vl_caminho_org
        ss_destination_filename = vl_caminho_novo
        ss_delete_source        = 'X'
        ss_presentation_server  = 'X'
        ss_application_server   = 'X'
      exceptions
        copy_error              = 1
        others                  = 2.

***************************************************************************************
Saída (impressão) imediata (direto) em report writer:
data saida(4) value 'ZEBR'.

data: c_copias type i value '1',               " Número de cópias
      v_flag_imed     value 'X',               " Flag de impressão
      v_flag_keep     value 'X',               " Deixa no spool
      c_dias type i   value '0'.               " No. de dias no spool

new-page print on
  destination saida
  copies c_copias
  immediately v_flag_imed
  keep in spool v_flag_keep
  dataset expiration c_dias.
*  NO DIALOG.

---------------------------------------------
      NEW-PAGE PRINT ON NEW-SECTION PARAMETERS params NO DIALOG.
****************************************************************************
status ordem produção:

Transação COR2, COR3 - Campo Status:

select single t~txt30
into lv_text
from jest as jest
inner join tj02t as t
on jest~stat = t~istat
where jest~objnr eq CAUFV-OBJNR "Acessar a tabela CAUFV com o nº da ordem
and jest~INACT eq space
and t~spras eq sy-langu
and t~txt04 eq 'ENTE'. "Status: Encerrado Tecnicamente

------ a função abaixo não pode ser executada dentro de exits, badis, pois tem 'commit':
TYPES: BEGIN OF ty_aufk,
       aufnr LIKE aufk-aufnr,
       objnr LIKE aufk-objnr,
       auart LIKE aufk-auart,
       END OF ty_aufk.

DATA: t_aufk TYPE TABLE OF ty_aufk WITH HEADER LINE,
      v_line LIKE bsvx-sttxt.

      CALL FUNCTION 'STATUS_TEXT_EDIT'
        EXPORTING
          objnr            = t_aufk-objnr
          spras            = sy-langu
        IMPORTING
          line             = v_line
        EXCEPTIONS
          object_not_found = 1
          OTHERS           = 2.

      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
                   WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

      MOVE v_line TO t_relatorio-status.

****************************************************************************
  DATA:
    L_STAT        LIKE JSTAT,
    L_STAT_TAB    LIKE JSTAT OCCURS 0,
    L_QAMB        LIKE QAMB,
    L_UPDKZ       LIKE QALSVB-UPSL VALUE 'U'.

*/QAMBs umsetzen (7 = VE-Buchung storniert)
  LOOP AT P_QAMB_TAB INTO L_QAMB.
    L_QAMB-TYP = '7'.
    APPEND L_QAMB TO P_QAMB_VB_TAB.
  ENDLOOP.

*/BERF & BTEI zurücknehmen
  CLEAR L_STAT. CLEAR L_STAT_TAB.
  L_STAT-INACT = 'X'.
  L_STAT-STAT = 'I0219'. APPEND L_STAT TO L_STAT_TAB. "BTEI
  L_STAT-STAT = 'I0220'. APPEND L_STAT TO L_STAT_TAB. "BEND

  CALL FUNCTION 'STATUS_CHANGE_INTERN'
       EXPORTING
            OBJNR         = P_QALS-OBJNR
       TABLES
            STATUS        = L_STAT_TAB
       EXCEPTIONS
            ERROR_MESSAGE = 1.

  IF SY-SUBRC <> 0.
    MESSAGE ID SY-MSGID TYPE 'S' NUMBER SY-MSGNO
            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    SUBMIT (SY-REPID) VIA SELECTION-SCREEN.
  ENDIF.

*/Prüflos aktualisieren
  CALL FUNCTION 'QPL1_UPDATE_MEMORY'
       EXPORTING
            I_QALS  = P_QALS
            I_UPDKZ = L_UPDKZ.

  CALL FUNCTION 'QPL1_INSPECTION_LOTS_POSTING'
       EXPORTING
              I_MODE    = '1'.

  CALL FUNCTION 'STATUS_UPDATE_ON_COMMIT'.

*/QAMB initialisieren
  CALL FUNCTION 'QAMB_REFRESH_DATA'.

  PERFORM UPDATE_QAMB ON COMMIT.

FORM UPDATE_QAMB.

  CALL FUNCTION 'QEVA_QAMB_CANCEL' IN UPDATE TASK
       EXPORTING
            T_QAMB_TAB = G_QAMB_VB_TAB.

ENDFORM.                               " UPDATE_QAMB


****************************************************************************
Fotos HR


  if handle is not initial.
    CALL FUNCTION 'HR_IMAGE_RESET'
      EXPORTING
        HANDLE = HANDLE.
  endif.

  CALL FUNCTION 'HR_IMAGE_INIT'
       EXPORTING
            P_PERNR       = edt_pernr
            P_TCLAS       = 'A'
            BEGDA         = '18000101'
            ENDDA         = '99991231'
            SIZE_X        = 100
            SIZE_Y        = 60
            OFFSET_X      = 0
            PROGRAM       = SY-REPID    "diese FuGr       "XYVAHRK024975
            DYNPRO_NUMBER = '0200'                          "Dynpro 100
*              URL           = PARAM-URL
            CONTAINER     = 'EDT_FOTO'
       IMPORTING
            HANDLE        = HANDLE.
*              SUBRC         = L_SUBRC.

****************************************************************************

Criar LOG STANDARD

Exemplo na função: J_1B_NFE_ERROR_PROTOKOLL.

****************************************************************************
Posicionar cursor em campo determinado na tela:
No PBO.
MODULE cursor OUTPUT.
  SET CURSOR FIELD name OFFSET pos.   "pos -- variável tipo 'i' --- posição no campo focado
ENDMODULE.

****************************************************************************
DATA: v_invdate TYPE j_1btxpis-validfrom.   "CHAR(8)

CONVERT DATE SY-DATUM INTO INVERTED-DATE V_invdate.

    SELECT * FROM j_1btxpis INTO ls_txpis
    WHERE country          =  uv_country
           AND gruop       = uv_gruop
           AND validfrom GE V_invdate
           AND validto   LE V_invdate.

****************************************************************************

Transforma tempo de segundos para formato hh:mm:ss
 call function 'MONI_TIME_CONVERT'
       EXPORTING
            ld_duration        = gd-duration      "403        ------> segundos
       IMPORTING
            lt_output_duration = gd-runtime.      "retorna: 00:06:43   (6 minutos)

****************************************************************************
Reiniciar contagem para time out:

call function 'TH_REDISPATCH'.

****************************************************************************
(restringir seleções) Alterar abas de campos 'select-options' da tela de seleção...

REPORT  ZTXT.

*Include type pool SSCR
TYPE-POOLS sscr.
TABLES : marc.

*Define the object to be passed to the RESTRICTION parameter
DATA restrict TYPE sscr_restrict.

*Auxiliary objects for filling RESTRICT
DATA : optlist TYPE sscr_opt_list,
ass TYPE sscr_ass.

*Defining the selection-screen
SELECT-OPTIONS : s_matnr FOR marc-matnr,
s_werks FOR marc-werks.

INITIALIZATION.

*Restricting the MATNR selection to only EQ and 'BT'.
optlist-name = 'OBJECTKEY1'.
optlist-options-eq = 'X'.
*optlist-options-bt = 'X'.
APPEND optlist TO restrict-opt_list_tab.

ass-kind = 'S'.
ass-name = 'S_MATNR'.
ass-sg_main = 'I'.
ass-sg_addy = space.
ass-op_main = 'OBJECTKEY1'.
APPEND ass TO restrict-ass_tab.

**Restricting the WERKS selection to CP, GE, LT, NE.
*optlist-name = 'OBJECTKEY2'.
*optlist-options-cp = 'X'.
*optlist-options-ge = 'X'.
*optlist-options-lt = 'X'.
*optlist-options-ne = 'X'.
*APPEND optlist TO restrict-opt_list_tab.
*
*ass-kind = 'S'.
*ass-name = 'S_WERKS'.
*ass-sg_main = 'I'.
*ass-sg_addy = space.
*ass-op_main = 'OBJECTKEY2'.
*APPEND ass TO restrict-ass_tab.

CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
EXPORTING
restriction = restrict
EXCEPTIONS
too_late = 1
repeated = 2
selopt_without_options = 3
selopt_without_signs = 4
invalid_sign = 5
empty_option_list = 6
invalid_kind = 7
repeated_kind_a = 8
OTHERS = 9.

IF sy-subrc <> 0.
MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
ENDIF.

****************************************************************************
Data de vencimento... pela condição de pagamento:

  DATA : top_text TYPE TABLE OF vtopis WITH HEADER LINE.

  CALL FUNCTION 'SD_PRINT_TERMS_OF_PAYMENT'
    EXPORTING
      bldat                        = bseg-zfbdt
      budat                        = bseg-zfbdt
      language                     = sy-langu
      terms_of_payment             = zterm
    TABLES
      top_text                     = top_text
    EXCEPTIONS
      terms_of_payment_not_in_t052 = 1
      OTHERS                       = 2.

  IF NOT top_text[] IS INITIAL.
    READ TABLE top_text INDEX 1 .
    MOVE top_text-hdatum TO zfbdt.

  ENDIF.

****************************************************************************
-Crie uma tabela interna (t_auxiliar), idêntica a que vc está usando para receber os dados de documentos de venda.
-Move a tabela interna (da qual vc buscou os pedidos de venda), para uma tabela auxiliar.
  Isto é para questão de performance. assim:
  t_auxiliar[] = sua_tabela[].  "no lugar de sua_tabela, coloca o nome da sua tabela interna.
 sort t_auxiliar by matnr.
 "elimina os registros duplicados (por material)
 delete adjacent duplicates from t_auxiliar comparing matnr.

"Busca o nr do imobilizado pelo documento de compra
  SELECT ekpo~matnr
         anla~anln1
         anla~anln2
   INTO TABLE t_anla FROM ekpo
   INNER JOIN ekkn ON
                ekpo~ebeln = ekkn~ebeln AND
                ekpo~ebelp = ekkn~ebelp
   INNER JOIN anla ON
                anla~anln1 = ekkn~anln1 AND
                anla~anln2 = ekkn~anln2
   FOR ALL entries IN t_auxiliar
   WHERE ekpo~matnr EQ t_auxiliar-matnr.

sort t_anla by matnr.

campo 'anln1' ---> nr imbolizado
campo 'anln2' ---> sub nr

****************************************************************************
Buscar dados parceiro na NF (cliente, fornecedor, local de negócios)

data: wa_parnad   like j_1binnad.
field-symbols <func_name>.

  clear: wa_parnad.
  read table i_1bac into j_1bac
                    with key partyp = wa_j1bnflin-partyp
                    binary search.

Tabela 'j_1bac':
*PARTYP FUNPAR                  
*B      J_1B_NF_BRANCH_READ     
*C      J_1B_NF_CUSTOMER_READ   
*V      J_1B_NF_VENDOR_READ     


* assign function name -------------------------------------------------
  assign j_1bac-funpar  to <func_name>.

* call function --------------------------------------------------------
  call function <func_name>
    exporting
      partner_id        = wa_j1bnflin-parid
    importing
      parnad            = wa_parnad
    exceptions
      partner_not_found = 01.
  if sy-subrc <> 0.
    call function 'J_1B_NF_PARTNER_READ'
      exporting
        partner_type     = wa_j1bnflin-partyp
        partner_id       = wa_j1bnflin-parid
        partner_function = wa_j1bnflin-parvw
        doc_number       = wa_j1bnflin-docnum
      importing
        parnad           = wa_parnad
      exceptions
        others           = 04.
  endif.

****************************************************************************
Rotina para buscar os objetos chamadores. Includes, programas, funções que foram chamadas antes do ponto onde se encontra.

data: li_callstack type abap_callstack,
      lw_callstack type abap_callstack_line.

call function 'SYSTEM_CALLSTACK'
  importing
    callstack = li_callstack.

read table li_callstack into lw_callstack
     with key mainprogram = 'SAPLV60A'.

if sy-subrc eq 0.
  assign ('(SAPLV60A)XVBRK[]') to <fs_xvbrk>.
  assign ('(SAPLV60A)XVBUK[]') to <fs_xvbuk>.
endif.

****************************************************************************
* Calcula diferença de horas (valor inteiro)
        CALL FUNCTION 'ISH_TIME_DIFFERENCE'
          EXPORTING
            date1                  = sy-datum
            time1                  = sy-uzeit
            date2                  = sy-datum
            time2                  = t_zwmt015-hrreserv
         IMPORTING
           timediff               = v_hora
*       EARLIEST               =
         EXCEPTIONS
           invalid_datetime       = 1
           OTHERS                 = 2.

---------------------
Retornar diferença em segundos ou horas (com casa decimal --- por exemplo: 23,50 horas)
report  ztstx.

parameters: p_data     type sy-datum,
            p_hora     type sy-uzeit.

data: v_aux(20)   type c,
      v_tempo_ini type timestamp,
      v_tempo_fim type timestamp,
      v_horas     type p decimals 2,
      v_secs      type i,
      v_hours     type i.

start-of-selection.

  "Data e hora de autorização da Nfe...
  concatenate p_data p_hora into v_aux.
  v_tempo_ini = v_aux.

  "Data e hora atual...
  get time stamp field v_tempo_fim.

  clear v_secs.
  "Calcula diferença de tempo...
  call function 'IGN_TIMESTAMP_DIFFERENCE'
    exporting
      i_timestamp1 = v_tempo_fim      "Data e hora atual...
      i_timestamp2 = v_tempo_ini      "Data e hora autorização da NFe...
      i_unit       = 'S'              "Retornar valor em 'segundos'
    importing
      e_diff_secs  = v_secs           "Diferença de tempo em 'segundos'.
      e_diff_hours = v_hours.         "Diferença de tempo em 'segundos'.

  v_horas = v_secs / 3600.    "Transforma os segundos, em horas...
  write: /01 'Data e hora inicial:', p_data, p_hora.
  write: /01 'Data e hora sistema:', sy-datum, sy-uzeit.
  write: /01 'Diferença em horas :', v_horas.
  if v_horas > '23.5'.
    "Se quantidade de horas for maior que 24, não permitir cancelar NFe.
    write: /01 'Ultrapassou limite de 23 horas e meia'.
  endif.

****************************************************************************
VRSV - gravar
VRGT - chamar

Verifica se variante existe:
    CALL FUNCTION 'RS_VARIANT_EXISTS'

Salva:
    CALL FUNCTION 'RS_CREATE_VARIANT'

popup com variantes do programa:
function RS_VARIANT_CATALOG

Busca parâmetros da variante do programa (retorna tabela interna com campos e valores):
    CALL FUNCTION 'RS_VARIANT_VALUES_TECH_DATA'

Deleta:
 CALL FUNCTION 'RS_VARIANT_DELETE'

****************************************************************************
VT03N ---- Alterar valores ALV 'Transportes e fornecimentos'
Exit: EXIT_SAPLV56A_003

types: ty_vtrlk     like vtrlk.
types: ty_vtrlp     like vtrlp.
types: ty_vtrlk_tab type standard table of ty_vtrlk.
types: ty_vtrlp_tab type standard table of ty_vtrlp.


field-symbols: <it_vtrlk_tab> type ty_vtrlk_tab,
               <it_vtrlp_tab> type ty_vtrlp_tab,
               <wa_vtrlk>     type ty_vtrlk,
               <wa_vtrlp>     type ty_vtrlp.


ranges: r_tcode for zes_range.


data: begin of it_lips occurs 0,
       vbeln  type lips-vbeln,
       kvgr2  type lips-kvgr2,
      end of it_lips.

data: begin of it_vbrp occurs 0,
       vbeln  type vbrp-vbeln,
       posnr  type vbrp-posnr,
       fkimg  type vbrp-fkimg,
       vrkme  type vbrp-vrkme,
       vgbel  type vbrp-vgbel,
       vgpos  type vbrp-vgpos,
      end of it_vbrp.

data: begin of it_refkey occurs 0,
       refkey  type j_1bnflin-refkey,
       refitm  type j_1bnflin-refitm,
      end of it_refkey.

data: begin of it_j1bnfdoc occurs 0,
       docnum  type j_1bnfdoc-docnum,
       refkey  type j_1bnflin-refkey,
       refitm  type j_1bnflin-refitm,
       nfnum   type j_1bnfdoc-nfnum,
       nfenum  type j_1bnfdoc-nfenum,
       series  type j_1bnfdoc-series,
       nfe     type j_1bnfdoc-nfe,
      end of it_j1bnfdoc.

data: v_qtd_cx type vbrp-fkimg.


  statics: rs_tcode type range of zes_range,
           tl_vtrlk type sorted table of ty_vtrlk
             with non-unique key vbeln.

  data: tl_vtrlk_aux type ty_vtrlk_tab,
        tl_vtrlp_aux type ty_vtrlp_tab.

  data: wa_vtrlk type ty_vtrlk.

  field-symbols: <wa_vtrlk_aux> type ty_vtrlk.

  if rs_tcode[] is initial.
    call function 'ZGE001TF02'
      exporting
        i_tipo          = 'RG'
        i_chave1        = 'Z_ZXV56U38_001'
        i_chave2        = ''
      tables
        t_range         = rs_tcode
      exceptions
        param_not_found = 1
        param_empty     = 2
        others          = 3.

    if sy-subrc <> 0.
      exit.
    endif.
  endif.

  if sy-tcode in rs_tcode.

    assign ('(SAPMV56A)XTRLK[]') to <it_vtrlk_tab>.

    if <it_vtrlk_tab> is assigned.

      assign ('(SAPMV56A)XTRLP[]') to <it_vtrlp_tab>.

      if <it_vtrlp_tab> is assigned.

        check <it_vtrlp_tab> is not initial.
        check <it_vtrlk_tab> is not initial.

        tl_vtrlp_aux = <it_vtrlp_tab>[].

        loop at <it_vtrlk_tab> assigning <wa_vtrlk>.
          read table tl_vtrlk
            with key vbeln = <wa_vtrlk>-vbeln
            binary search
            transporting no fields.

          if sy-subrc ne 0 or <wa_vtrlk>-spaiv is initial.
            append <wa_vtrlk>
              to tl_vtrlk_aux.
          else.
            delete tl_vtrlp_aux
              where vbeln eq <wa_vtrlk>-vbeln.
          endif.
        endloop.

        check not tl_vtrlk_aux is initial.
        check not tl_vtrlp_aux is initial.

        sort tl_vtrlp_aux by vbeln.

        select vbeln kvgr2
          from lips
          into table it_lips
          for all entries in tl_vtrlk_aux
          where vbeln eq tl_vtrlk_aux-vbeln.

        sort: it_lips by vbeln.

        "Selec para buscar o numero da remessa
        select vbeln posnr fkimg vrkme vgbel vgpos
          from vbrp
          into table it_vbrp
          for all entries in tl_vtrlp_aux
          where vgbel eq tl_vtrlp_aux-vbeln
            and vgpos eq tl_vtrlp_aux-posnr.

        sort: it_vbrp by vgbel vgpos.

        loop at it_vbrp.
          it_refkey-refkey  =  it_vbrp-vbeln.
          it_refkey-refitm  =  it_vbrp-posnr.
          append it_refkey.
        endloop.

        if it_refkey[] is not initial.
          "Select para buscar o numero da nota fiscal
          select doc~docnum lin~refkey lin~refitm doc~nfnum
                 doc~nfenum doc~series doc~nfe
            into table it_j1bnfdoc
            from j_1bnflin as lin
            inner join j_1bnfdoc as doc
               on lin~docnum = doc~docnum
            for all entries in it_refkey
            where lin~refkey = it_refkey-refkey
              and lin~refitm = it_refkey-refitm.
        endif.

        sort: it_j1bnfdoc by refkey refitm.

        loop at tl_vtrlk_aux assigning <wa_vtrlk>.

          read table tl_vtrlp_aux
            assigning <wa_vtrlp>
            with key vbeln = <wa_vtrlk>-vbeln
            binary search.

          if sy-subrc eq 0.

            "**** Setor de Atividade ****"
            <wa_vtrlk>-spaiv = <wa_vtrlp>-spart.
            "**** Setor de Atividade ****"

            "**** ROTA ****"
            read table it_lips with key vbeln = <wa_vtrlk>-vbeln
                                        binary search.
            if sy-subrc eq 0.
              <wa_vtrlk>-lland = it_lips-kvgr2.
            endif.
            "**** ROTA ****"

            "**** Numero da Nota Fiscal ****"
            read table it_vbrp with key vgbel = <wa_vtrlk>-vbeln
                                        binary search.
            if sy-subrc eq 0.

              "**** Quantidade Em caixa ****"
              clear: v_qtd_cx.
              data: lw_vbrp like line of it_vbrp.

              "Loop usando work area, para não perder o read
              loop at it_vbrp into lw_vbrp from sy-tabix.
                if lw_vbrp-vgbel ne <wa_vtrlk>-vbeln.
                  exit.
                endif.

                v_qtd_cx = v_qtd_cx + lw_vbrp-fkimg.
              endloop.

              write: v_qtd_cx to <wa_vtrlk>-labnk.
              "**** Quantidade Em caixa ****"

              "Read usando header line, pois o loop anterior pode perder a referencia
              read table it_j1bnfdoc with key refkey = it_vbrp-vbeln
                                              binary search.
              if sy-subrc eq 0.
                if it_j1bnfdoc-nfe is initial.
                  "NF normal
                  concatenate it_j1bnfdoc-nfnum it_j1bnfdoc-series
                         into <wa_vtrlk>-qmnum separated by '-'.
                else.
                  "NF eletronia
                  concatenate it_j1bnfdoc-nfenum it_j1bnfdoc-series
                         into <wa_vtrlk>-qmnum separated by '-'.
                endif.
              endif.
            endif.
            "**** Numero da Nota Fiscal ****"


            wa_vtrlk = <wa_vtrlk>.

            insert wa_vtrlk
              into table tl_vtrlk.

            read table <it_vtrlk_tab>
              assigning <wa_vtrlk_aux>
              with key vbeln = wa_vtrlk-vbeln.

            if sy-subrc eq 0.
              <wa_vtrlk_aux> = wa_vtrlk.
            endif.

          endif.

        endloop.

      endif.

    endif.

  endif.

****************************************************************************
Simple Code to consume Web service using SAP ABAP
Below is very simple code, which demonstration how to call a webservice using class CL_HTTP_CLIENT. Web service used in this example is GetCitiesByCountry which returns all major cities by country name. It returns the result in xml format. The request is invoke in form http://www.webservicex.net/globalweather.asmx/GetCitiesByCountry?CountryName=india. 

REPORT  zpw_webservice.
 
*&---------------------------------------------------------------------*
*&      Selection Screen
*&---------------------------------------------------------------------*
PARAMETERS : p_cnt TYPE t005t-landx .
 
*&---------------------------------------------------------------------*
*&      Types and Data
*&---------------------------------------------------------------------*
DATA:  http_client    TYPE REF TO if_http_client ,
http_url       TYPE string                ,
p_content      TYPE string                .
 
*&---------------------------------------------------------------------*
*&      Start of Selection
*&---------------------------------------------------------------------*
START-OF-SELECTION .
 
* Build the url string based on input
CONCATENATE 'http://www.webservicex.net/globalweather.asmx'
'/GetCitiesByCountry?CountryName='
p_cnt
INTO http_url .
 
* Creation of new IF_HTTP_Client object
CALL METHOD cl_http_client=>create_by_url
EXPORTING
url                = http_url
IMPORTING
client             = http_client
EXCEPTIONS
argument_not_found = 1
plugin_not_active  = 2
internal_error     = 3
OTHERS             = 4.
 
http_client->request->set_header_field( name  = '~request_method'
value = 'GET' ).
* Send the request
http_client->send( ).
 
* Reterive the result
CALL METHOD http_client->receive
EXCEPTIONS
http_communication_failure = 1
http_invalid_state         = 2
http_processing_failed     = 3
OTHERS                     = 4.
 
p_content = http_client->response->get_cdata( ).
REPLACE  ALL OCCURRENCES OF '<' IN p_content WITH '<' .
REPLACE ALL OCCURRENCES OF '>' IN p_content WITH '>' .
 
*&---------------------------------------------------------------------*
*&      Processing the string
*&---------------------------------------------------------------------*
DATA : moff  TYPE syst-tabix ,
moff1 TYPE syst-tabix ,
len   TYPE syst-tabix .
 
DO .
   FIND '<City>' IN SECTION OFFSET moff OF p_content IGNORING CASE MATCH OFFSET moff .
  IF sy-subrc = 0 .
    moff = moff + 6 .
    FIND '</City>' IN SECTION OFFSET moff OF p_content IGNORING CASE MATCH OFFSET moff1 .
    len = moff1 - moff .
    WRITE : / p_content+moff(len) .
  ELSE.
    EXIT.
  ENDIF.
 
ENDDO .

****************************************************************************
* Verifica se o campo é Numérico

data: v_htype(04) type c.

    perform checa_se_numerico using v_valor.

form checa_se_numerico using p_campo.

  clear v_htype.
  call function 'NUMERIC_CHECK'
    exporting
      string_in = p_campo
    importing
      htype     = v_htype.

  if v_htype ne 'NUMC'.
    p_campo     = '1'.
  endif.

endform.

****************************************************************************
Função que exibe onde é usado parameter ID

RS_PARAMETER_SHOW (só colocar o nome do ID), executar, e clicar em lista de utilizações.

****************************************************************************
Transação smw0

Padrões HTML

Criar um Z... criar um documento HTML e importar na smw0

****************************************************************************
Retorna/recalcula Conditions pedido de compra:

FUNCTION z_calc_imposto.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IN_EKKO) TYPE  EKKO
*"     REFERENCE(IN_EKPO) TYPE  EKPO
*"     REFERENCE(IN_FORNEC) TYPE  LFA1
*"     REFERENCE(IN_WAERS) TYPE  WAERS
*"  EXPORTING
*"     REFERENCE(I_TAXCOM) TYPE  TAXCOM
*"  TABLES
*"      T_KOMV STRUCTURE  KOMV
*"----------------------------------------------------------------------
  DATA: e_jtaxcom TYPE j_1b_taxcom,
        e_taxcom  TYPE taxcom.


  e_jtaxcom-txreg_sf = in_fornec-txjcd.
  e_jtaxcom-txreg_st = in_ekpo-txjcd.
  e_jtaxcom-taxbs    = in_fornec-taxbs.
  e_jtaxcom-ipisp    = in_fornec-ipisp.
  e_jtaxcom-brsch    = in_fornec-brsch.
  e_jtaxcom-mtuse    = in_ekpo-j_1bmatuse.
  e_jtaxcom-mtorg    = in_ekpo-j_1bmatorg.
  e_jtaxcom-ownpr    = in_ekpo-j_1bownpro.
  e_jtaxcom-steuc    = in_ekpo-j_1bnbm.
  e_jtaxcom-matkl    = in_ekpo-matkl.
  e_jtaxcom-vrkme    = in_ekpo-meins.
  e_jtaxcom-mgame    = in_ekpo-menge.


* ISS Calculation with 2-level tax jurisdiction code
* Location of service provider = Tax Jur. Code of
* vendor:
  e_jtaxcom-loc_pr   = in_fornec-txjcd.
* Location of service = Tax Jur. Code of delivery address

*Populate fields based on country
  CALL FUNCTION 'J_1B_SAVE_TAX_FIELDS'
    EXPORTING
      i_taxcom = e_jtaxcom.

*  CALL FUNCTION 'J_1B_NF_PO_DISCOUNTS'
*    EXPORTING
*      i_kalsm = in_ekko-kalsm
*      i_ekpo  = in_ekpo
*    TABLES
*      i_konv  = t_konv.


  CLEAR e_taxcom.
  e_taxcom-bukrs = in_ekpo-bukrs.
  e_taxcom-budat = in_ekko-bedat.
  e_taxcom-waers = in_ekko-waers.
  e_taxcom-kposn = in_ekpo-ebelp.
  e_taxcom-mwskz = in_ekpo-mwskz.
  e_taxcom-txjcd = in_ekpo-txjcd.
  e_taxcom-shkzg = 'H'.
  e_taxcom-xmwst = 'X'.
  IF in_ekko-bstyp EQ 'F'.
    e_taxcom-wrbtr = in_ekpo-netwr.
  ELSE.
    e_taxcom-wrbtr = in_ekpo-zwert.
  ENDIF.
  e_taxcom-lifnr = in_ekko-lifnr.
  e_taxcom-land1 = in_ekko-lands. "WIA
  e_taxcom-ekorg = in_ekko-ekorg.
  e_taxcom-hwaer = in_waers.
  e_taxcom-llief = in_ekko-llief.
  e_taxcom-bldat = in_ekko-bedat.
  e_taxcom-matnr = in_ekpo-matnr. "HTN-Abwicklung
  e_taxcom-werks = in_ekpo-werks.
  e_taxcom-bwtar = in_ekpo-bwtar.
  e_taxcom-matkl = in_ekpo-matkl.
  e_taxcom-meins = in_ekpo-meins.
  IF in_ekko-bstyp EQ 'F'.
    e_taxcom-mglme = in_ekpo-menge.
  ELSE.
    IF in_ekko-bstyp EQ 'K' AND in_ekpo-abmng GT 0.
      e_taxcom-mglme = in_ekpo-abmng.
    ELSE.
      e_taxcom-mglme = in_ekpo-ktmng.
    ENDIF.
  ENDIF.
  IF e_taxcom-mglme EQ 0.
    e_taxcom-mglme = 1000.
  ENDIF.
  e_taxcom-mtart = in_ekpo-mtart.

  CHECK NOT e_taxcom-mwskz IS INITIAL.

  CALL FUNCTION 'CALCULATE_TAX_ITEM'
    EXPORTING
      dialog              = ' '
      display_only        = ' '
      i_taxcom            = e_taxcom
    IMPORTING
      e_taxcom            = e_taxcom
    TABLES
      t_xkomv             = t_komv
    EXCEPTIONS
      mwskz_not_defined   = 1
      mwskz_not_found     = 2
      mwskz_not_valid     = 3
      steuerbetrag_falsch = 4
      country_not_found   = 5
      OTHERS              = 6.


  i_taxcom = e_taxcom.

  CALL FUNCTION 'REFRESH_TAX_TABLES'.

  CALL FUNCTION 'PRICING_REFRESH_TX'.

****************************************************************************
Enviar email... alterar o nome do remetente:

Pessoal deu certo. Não coloquei o tipo 'SENDER_ADRESS_TYPE e foi.
 
Obrigado a todos.'
Em 16 de setembro de 2011 09:37, Marcos Sekiya <masekiya@gmail.com> escreveu:
Bom dia pessoal.
 
Obrigado pelas dicas.
 
Uma dúvida, tentei utilizar a função SO_DOCUMENT_SEND_API1 e ela não funcionou aqui.
 
Sabem me dizer se ela ainda funciona na versão 6.0. Tentei executar diretamente da SE37 e retornou a EXCEPTION 'X_ERROR', com a mensagem 'Elemento NOAUTHORITYCHECK não existe no container'.
 
Alguém sabe me dizer o que ocorreu?
 
Obrigado.
2011/9/15 Paiva <paivaluciano@hotmail.com>

Função SO_DOCUMENT_SEND_API1
Utilize os parametros:
SENDER_ADDRESS
SENDER_ADDRESS_TYPE
 

****************************************************************************
include <icon>.

field-symbols: <icone> like icon_checked.

assign ICON_GREEN_LIGHT to <icone>.

  write: /01 sy-vline,
          03 <icone> as icon,
          08 t_fornec-name1,
          63 p_message,
         150 sy-vline.

****************************************************************************
Abrir PDF:
SAP_PDF_VIEWER_DEMO.
Mudar o caminho\nome do arquivo pdf.
      call method my_pdf_viewer->open_document
        exporting
          "url = 'http://p34198/iissamples/default/ReadMe.pdf'.
          url = 'C:\tmp\Reader.pdf'.


Ou:

REPORT  ZSAP_PDF_VIEWER. 

DATA: LT_PDF TYPE TABLE OF TLINE,
      LS_PDF LIKE LINE OF LT_PDF,
      LV_URL TYPE CHAR255,
      PDF_FSIZE TYPE  I,
      LV_CONTENT  TYPE XSTRING,
      LT_DATA TYPE STANDARD TABLE OF X255.

DATA : L_JOB_OUTPUT_INFO TYPE SSFCRESCL.
DATA : LS_CONTROL_PARAM  TYPE SSFCTRLOP.

DATA : G_HTML_CONTAINER TYPE REF TO CL_GUI_CUSTOM_CONTAINER,
       G_HTML_CONTROL   TYPE REF TO CL_GUI_HTML_VIEWER.

DATA : P_VBELN TYPE  VBELN_VL.

FIELD-SYMBOLS <FS_X> TYPE X.

INITIALIZATION.
LS_CONTROL_PARAM-GETOTF = 'X'.
LS_CONTROL_PARAM-NO_DIALOG = 'X'.
START-OF-SELECTION.

  CALL FUNCTION '/1BCDWB/SF00000034'
 EXPORTING
*    ARCHIVE_INDEX              =
*   ARCHIVE_INDEX_TAB          =
*   ARCHIVE_PARAMETERS         =
     CONTROL_PARAMETERS         = LS_CONTROL_PARAM
     P_VBELN                    = P_VBELN
*   MAIL_APPL_OBJ              =
*   MAIL_RECIPIENT             =
*   MAIL_SENDER                =
*   OUTPUT_OPTIONS             =
*   USER_SETTINGS              = 'X'
 IMPORTING
*      DOCUMENT_OUTPUT_INFO  = L_DOCUMENT_OUTPUT_INFO
       JOB_OUTPUT_INFO       = L_JOB_OUTPUT_INFO
*      JOB_OUTPUT_OPTIONS    = L_JOB_ OUTPUT_OPTIONS
 EXCEPTIONS
    FORMATTING_ERROR           = 1
    INTERNAL_ERROR             = 2
    SEND_ERROR                 = 3
    USER_CANCELED              = 4
    OTHERS                     = 5
           .
  IF SY-SUBRC  <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  CALL FUNCTION 'CONVERT_OTF'
    EXPORTING
      FORMAT                = 'PDF'
    IMPORTING
      BIN_FILESIZE          = PDF_FSIZE
    TABLES
      OTF                   = L_JOB_OUTPUT_INFO-OTFDATA
      LINES                 = LT_PDF
    EXCEPTIONS
      ERR_MAX_LINEWIDTH     = 1
      ERR_FORMAT            = 2
      ERR_CONV_NOT_POSSIBLE = 3
      OTHERS                = 4.

* convert pdf to xstring string
  LOOP AT LT_PDF INTO LS_PDF.
    ASSIGN LS_PDF TO <FS_X> CASTING.
    CONCATENATE LV_CONTENT <FS_X> INTO LV_CONTENT IN BYTE MODE.
  ENDLOOP.

  CALL SCREEN 100.
MODULE STATUS_0100 OUTPUT.
*  SET PF-STATUS 'xxxxxxxx'.
*  SET TITLEBAR 'xxx'.
  CREATE OBJECT G_HTML_CONTAINER
    EXPORTING
      CONTAINER_NAME = 'PDF'.
 CREATE OBJECT G_HTML_CONTROL
    EXPORTING
      PARENT = G_HTML_CONTAINER.
* Convert xstring to binary table to pass to the LOAD_DATA method
  CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
    EXPORTING
      BUFFER     = LV_CONTENT
    TABLES
      BINARY_TAB = LT_DATA.

* Load the HTML
  CALL METHOD G_HTML_CONTROL->LOAD_DATA(
     EXPORTING
       TYPE         = 'application'
       SUBTYPE      = 'pdf'
     IMPORTING
       ASSIGNED_URL         = LV_URL
     CHANGING
       DATA_TABLE           = LT_DATA
     EXCEPTIONS
       DP_INVALID_PARAMETER = 1
       DP_ERROR_GENERAL     = 2
       CNTL_ERROR           = 3
       OTHERS               = 4 ).

* Show it
  CALL METHOD G_HTML_CONTROL->SHOW_URL( URL = LV_URL
    IN_PLACE = 'X' ).

ENDMODULE.                 " STATUS_0100  OUTPUT
MODULE USER_COMMAND_0100 INPUT.

ENDMODULE.                 " USER_COMMAND_0100  INPUT 

****************************************************************************
Incoerência na mensagem de erro, transação MSC2N:
Em regra de validações ou user exits.
      "Quando transação MSC2N, não apresenta a mensagem de erro coerente.
      "Está apresentando o erro 'standard' M7 661, que ocorre qdo
      "sai da regra de validação (retornando FALSE),
      "e volta para a função chamadora VB_CHANGE_BATCH_VAL_STOCKS.
      "Para fazer com que exiba a mensagem abaixo, é necessário preencher os campos
      "da tabela interna 'standard' xemseg, que está contida na função citada acima (VB_CHANGE_BATCH_VAL_STOCKS).
      "msgid = zmm, msgno = '078', msgty = 'E', msgv1 = SYST-DATUM, msgv2 = BSEG-WERKS, msgv3 = BSEG-BUPLA,
      " msgv4 = BSEG-STCEG.
      if sy-tcode = 'MSC2N'.
        assign ('(SAPLV01Z)XEMSEG[]') to <fs_xemseg>.
        if <fs_xemseg> is assigned.
          xemseg[] = <fs_xemseg>.
          if not xemseg[] is initial.
            read table xemseg index 1.
            xemseg-msgid = 'ZFI'.
            xemseg-msgno = '078'.
            xemseg-msgty = 'E'.
            xemseg-msgv1 = sy-datum.
            xemseg-msgv2 = bseg-werks.
            xemseg-msgv3 = bseg-bupla.
            xemseg-msgv4 = bseg-stceg.
            modify xemseg index 1 transporting msgid msgno msgty msgv1 msgv2.
            <fs_xemseg> = xemseg[].
          endif.
        endif.
      endif.

*****************************************************************************************************
Nota para Etapa de autorização --- transporte de requests:

ED1K900821 (executar no eq1)

Tabela TMSQWLFH (buscar STEP):
DOMNAM     SYSNAM     TRKORR               CLIENT AS4DATE    AS4TIME  STEP
DOMAIN_ECD EQ1        ED1K900821           130    27.04.2012 14:41:33 SAP03


gc_tms_qas             LIKE stmsc-service  VALUE 'QAS',

SERVICE    NOTEKEY
QAS        ED1K90082113020120427144133SAP03

            refresh gt_dyn0200_notes.
            refresh gt_dyn0200_notekeys.
            gs_dyn0200_notekey-service = gc_tms_qas.
            read table gt_qaflags index gv_dyn0200_line.
            concatenate gt_qaflags-trkorr  "ED1K900821
                        gt_qaflags-client  "130
                        gt_qaflags-as4date "20120427
                        gt_qaflags-as4time "144133
                        gt_qaflags-step    "SAP03
                   into gs_dyn0200_notekey-notekey.
            append gs_dyn0200_notekey to gt_dyn0200_notekeys.


SAPLTMSQI                      / LTMSQII02
MODULE (PAI)                   / USER_COMMAND_0200


            call function 'TMS_QAM_NOTE'
                 exporting
                      iv_domain       = gs_system_config-domnam  "DOMAIN_ECD
                      iv_system       = gv_system                "EQ1
                      IV_NOTE_READ    = 'X'
                      IV_NOTE_SAVE    = space
                      it_notekeys     = gt_dyn0200_notekeys
                      IV_PROGRESS_MAX = 1
                      IV_QA_HISTORY   = gv_qa_history            "X"
*                IMPORTING
*                     ES_EXCEPTION    =
                 changing
                      ct_notes        = gt_dyn0200_notes  "Nesta, tem o registro contendo o título da etapa de                                                           "autorização
                 EXCEPTIONS
                      ALERT           = 1
                      OTHERS          = 2.
            if sy-subrc <> 0.
                MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
                WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
            else.
*****************************************************************************************************
Buscar programas, funções, etc... que utilizam determinado objeto de autorização:
Função RS_EU_CROSSREF
Parâmetro importação            Valor

I_FIND_OBJ_CLS                  B
I_SCOPE_OBJ_CLS
REKURSIV
I_ANSWER
I_ACTUAL_INCLUDE
NO_DIALOG                       X
EXPAND_SOURCE_IN_BATCH_MODE     X
EXPAND_SOURCE_IN_ONLINE_MODE
WITHOUT_TEXT
WITH_GENERATED_OBJECTS

Tables I_FINDSTRINGS  (inserir linha com o nome do objeto de autorização)

*****************************************************************************************************

Emitir SOM (voz):

      INCLUDE ole2incl.
      DATA objectvar1 TYPE ole2_object.
      DATA objvoice   TYPE ole2_object.
      DATA strtext    TYPE string.
      CREATE OBJECT objvoice 'SAPI.SpVoice'.
      strtext = 'Thank you very much! Bye Bye!'.
      CALL METHOD OF objvoice 'Speak' = objectvar1
        EXPORTING #0 = strtext.
      WAIT UP TO 1 SECONDS.
      FREE: objvoice,objectvar1.

*****************************************************************************************************
Busca email de usuários:
            " Busca o e-mail do aprovador
            CALL FUNCTION 'EFG_GEN_GET_USER_EMAIL'
              EXPORTING
                i_uname           = it_fw-objid
              IMPORTING
                e_email_address   = v_email
              EXCEPTIONS
                not_qualified     = 1
                user_not_found    = 2
                address_not_found = 3
                OTHERS            = 4.

*****************************************************************************************************
Para capturar valor de um campo na tela de seleção, em query:
        field-symbols: <fs_data> type any.
        assign ('SP$00001') to <fs_data>.
        if ( <fs_data> is assigned )
------------------------
Transação (se93) para query:
Just create a parameter transaction with reference to START_REPORT transaction. When creating a parameter transaction you have to set the following parameters:

transaction = START_REPORT
D_SREPOVARI-REPORTTYPE = AQ
D_SREPOVARI-REPORT = precisely the first 12 characters - query user group (including trailing spaces), 13-th character is G for global queries
D_SREPOVARI-EXTDREPORT = Query name as shown in SQ01.


That's it.
Well, and do not forget to check the flag "Skip first screen".

Para transportar query:
Vai no Infoset ---- Clica no botão <transportes> (caminhao) ------ Selecione 'Exportação' ---
deixe marcado somente 'Sobregravação permitida' ---- Escolha a opção 'Transporte de infosets e queries ---
entre com o nome do infoset e o nome da query --- Execute ---- Vai gerar uma request.

Para importar query na mesma instância:
Vai no Infoset ---- Clica no botão <transportes> (caminhao) ------ Selecione 'Importação' ---
deixe marcado somente 'Sobregravação permitida' --- em 'Conj.dados ao importar' entre com o nr da request --- Execute.

*****************************************************************************************************
ALV básico:
*--------------------------------------------------------------------*
* Autor   : Luismar A. C. Borges                                     *
* Data    : 19/04/2011                                               *
* Chamado : 148457                                                   *
* Módulo  : Perfil (Marta e Hariolo).                                *
* Objetivo: Listar modificações do cadastro usuários (SU01).         *
*--------------------------------------------------------------------*

report zperfr001 message-id zperf.

tables: ztperf001, ztperf002, usr02.

type-pools: slis.

data: begin of it_nome_tab occurs 0,
         tabname    like dd02t-tabname,
         ddtext     like dd02t-ddtext,
      end of it_nome_tab.

data: begin of it_nome_campo occurs 0,
         tabname    like dd03m-tabname,
         fieldname  like dd03m-fieldname,
         scrtext_m  like dd03m-scrtext_m,
      end of it_nome_campo.

data: begin of it_log occurs 0,
          objectid    like ztperf001-objectid,
          username    like ztperf001-username,
          udate       like ztperf001-udate,
          utime       like ztperf001-utime,
          tabname     like ztperf002-tabname,
          fname       like ztperf002-fname,
          chngind     like ztperf002-chngind,
          value_new   like ztperf002-value_new,
          value_old   like ztperf002-value_old,
       end of it_log.

data: begin of it_saida occurs 0,
          objectid    like ztperf001-objectid,
          username    like ztperf001-username,
          udate       like ztperf001-udate,
          utime       like ztperf001-utime,
          tabname     like ztperf002-tabname,
          tabname_t   like dd02t-ddtext,
          fname       like ztperf002-fname,
          fname_t     like dd03m-scrtext_m,
          chngind     like ztperf002-chngind,
          value_new   like ztperf002-value_new,
          value_old   like ztperf002-value_old,
       end of it_saida.

data: it_fieldcat    type slis_t_fieldcat_alv,
      it_log_aux     like table of it_log.

************************************************************************
* Work Areas                                                           *
************************************************************************
data: wa_fieldcat    like line of it_fieldcat.

************************************************************************
* Tela de seleção                                                      *
************************************************************************
selection-screen begin of block b1 with frame title text-t01.
select-options: s_user   for usr02-bname,
                s_udate  for ztperf001-udate,
                s_userm  for usr02-bname.
selection-screen end of block b1.

"Macros do programa
define barra_progresso.
  call function 'SAPGUI_PROGRESS_INDICATOR'
    exporting
      percentage = &1
      text       = &2.
end-of-definition.


start-of-selection.

  perform f_valida_tela.

  perform f_seleciona_dados.

  perform f_prepara_dados.

  perform f_imprime.

***--------------------------------------

form f_valida_tela.

  if s_udate[] is initial
         and s_user[] is initial
            and s_userm[] is initial.
    message i000(zperf)
     with 'Favor entrar com critério para seleção de dados!'.
    stop.
  endif.

endform.                    "f_valida_tela


form f_seleciona_dados.

  barra_progresso 45 text-002.
  "Aguarde! Selecionando dados...

  "Busca modificações, de acordo com entrada na tela de seleção.
  if not s_udate[] is initial.  "se entrou com data...
    select c~objectid c~username c~udate c~utime i~tabname i~fname
           i~chngind i~value_new i~value_old
          from ztperf001 as c
          inner join ztperf002 as i
                on c~objectclas = i~objectclas and
                   c~objectid   = i~objectid   and
                   c~changenr   = i~changenr
       into table it_log
         where     udate in s_udate
          and c~objectid in s_user
          and c~username in s_userm.
  elseif not s_user[] is initial. "se entrou com usuário modificado...
    select c~objectid c~username c~udate c~utime i~tabname i~fname
           i~chngind i~value_new i~value_old
          from ztperf001 as c
          inner join ztperf002 as i
                on c~objectclas = i~objectclas and
                   c~objectid   = i~objectid   and
                   c~changenr   = i~changenr
     into table it_log
       where c~objectid in s_user
              and udate in s_udate
              and c~username in s_userm.
  else. "se entrou com usuário que modificou...
    select c~objectid c~username c~udate c~utime i~tabname i~fname
           i~chngind i~value_new i~value_old
          from ztperf001 as c
          inner join ztperf002 as i
                on c~objectclas = i~objectclas and
                   c~objectid   = i~objectid   and
                   c~changenr   = i~changenr
     into table it_log
       where c~username in s_userm
         and c~objectid in s_user
         and      udate in s_udate.
  endif.

  if it_log[] is initial.
    message i001(zperf).
    stop.
  endif.

endform.

*&---------------------------------------------------------------------*
*&      Form  f_imprime
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form f_imprime.

  check not it_log[] is initial.

  perform f_monta_field.

  perform f_exibe_relatorio.

endform.                    "f_imprime

*&---------------------------------------------------------------------*
*&      Form  f_monta_field
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form f_monta_field.

  call function 'REUSE_ALV_FIELDCATALOG_MERGE'
    exporting
      i_program_name         = sy-repid
      i_internal_tabname     = 'IT_SAIDA'
      i_inclname             = sy-repid
    changing
      ct_fieldcat            = it_fieldcat
    exceptions
      inconsistent_interface = 1
      program_error          = 2
      others                 = 3.

  loop at it_fieldcat into wa_fieldcat.
    case wa_fieldcat-fieldname.
      when 'OBJECTID'.
        wa_fieldcat-hotspot = 'X'.
        wa_fieldcat-outputlen = 14.
        perform f_texto using text-h02
                 changing wa_fieldcat-seltext_l.
    endcase.
    modify it_fieldcat from wa_fieldcat.
  endloop.

endform.                    "f_monta_field

form f_texto using p_text.

   wa_fieldcat-seltext_l    = p_text.
   wa_fieldcat-seltext_m    = p_text.
   wa_fieldcat-seltext_s    = p_text.
   wa_fieldcat-reptext_ddic = p_text.

endform.

*&---------------------------------------------------------------------*
*&      Form  f_exibe_relatorio
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form f_exibe_relatorio.

  call function 'REUSE_ALV_GRID_DISPLAY'
    exporting
      i_callback_program      = sy-repid
      it_fieldcat             = it_fieldcat[]
      i_callback_user_command = 'F_USER_COMMAND'
    tables
      t_outtab                = it_saida.

endform.                    "f_exibe_relatorio

*&---------------------------------------------------------------------*
*&      Form  f_user_command
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_UCOMM    text
*      -->P_SELFIELD text
*----------------------------------------------------------------------*
form f_user_command using p_ucomm    like sy-ucomm
                          p_selfield type slis_selfield.

  case p_ucomm.
    when '&IC1'.
      "Verifica campo selecionado
      case p_selfield-sel_tab_field.
        when 'IT_SAIDA-OBJECTID'.
          "Posiciona no registro selecionado
          read table it_saida index p_selfield-tabindex.
          "Move valores para o parâmetro ID de memória
          set parameter id 'XUS' field it_saida-objectid.
          "Chamar transação SU01
          call transaction 'SU01'.   " and skip first screen.
        when 'IT_SAIDA-USERNAME'.
          "Posiciona no registro selecionado
          read table it_saida index p_selfield-tabindex.
          "Move valores para o parâmetro ID de memória
          set parameter id 'XUS' field it_saida-username.
          "Chamar transação SU01
          call transaction 'SU01'.   " and skip first screen.
      endcase.
  endcase.

endform.                    "f_user_command

*&---------------------------------------------------------------------*
*&      Form  f_prepara_dados
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form f_prepara_dados.

  "Busca textos de campos standards - Início
  it_log_aux[] = it_log[].
  sort it_log_aux by tabname fname.
  delete adjacent duplicates from it_log_aux comparing tabname fname.

  select tabname fieldname scrtext_m from dd03m
         into table it_nome_campo
              for all entries in it_log_aux
         where  tabname = it_log_aux-tabname
         and  fieldname = it_log_aux-fname
         and ddlanguage = sy-langu.

  sort it_nome_campo by tabname fieldname.
  "Busca textos de campos standards - Fim

  "Busca textos de tabelas standards - Início
  it_log_aux[] = it_log[].
  sort it_log_aux by tabname.
  delete adjacent duplicates from it_log_aux comparing tabname.

  select tabname ddtext from dd02t
         into table it_nome_tab
              for all entries in it_log_aux
         where  tabname = it_log_aux-tabname
         and ddlanguage = sy-langu.

  sort it_nome_tab by tabname.
  "Busca textos de tabelas standards - Fim

  loop at it_log.
    clear it_saida.
    move-corresponding it_log to it_saida.

    "Descrição da tabela...
    clear it_nome_tab.
    read table it_nome_tab with key tabname = it_log-tabname
                                               binary search.
    it_saida-tabname_t = it_nome_tab-ddtext.

    "Descrição do campo...
    clear it_nome_campo.
    read table it_nome_campo with key tabname = it_log-tabname
                                    fieldname = it_log-fname
                                               binary search.
    it_saida-fname_t = it_nome_campo-scrtext_m.

    append it_saida.

  endloop.

endform.                    "f_prepara_dados

*****************************************************************************************************
Um ALV em 10 minutos:

report ztmp no standard page heading.
tables: vttk.
data :ref_alv       type ref to cl_gui_alv_grid,
      ts_vttk       type table of vttk.

selection-screen begin of block b1.
  select-options: s_tknum for vttk-tknum.
selection-screen end of block b1.

select * from vttk into table ts_vttk up to 100 rows.
create object ref_alv
    exporting
       i_parent = cl_gui_container=>screen0.

call method ref_alv->set_table_for_first_display
     exporting
         i_structure_name = 'VTTK'
     changing
         it_outtab        = ts_vttk.

call selection-screen 1000.

*****************************************************************************************************
Desabilitar (disable) célula editável em ALV.

report  zteste3.
tables :t247.

type-pools slis. "Type definitions for alv report

data: it_fieldcat type lvc_t_fcat,
wa_fieldcat type lvc_s_fcat.

data: wa_layout type lvc_s_layo.

data : begin of it_final occurs 0,
check(1),
celltab type lvc_t_styl, " Switch between display/change
mnr like t247-mnr,
ltx like t247-ltx,
end of it_final.
data : wa_final like it_final.
data : w_repid like sy-repid.

w_repid = sy-repid.

refresh it_final.
select mnr ltx
from t247
into corresponding fields of table it_final
where spras eq 'E'.


wa_fieldcat-fieldname = 'CHECK'.
wa_fieldcat-tabname = 'IT_FINAL'.
wa_fieldcat-checkbox = 'X'.
wa_fieldcat-edit = 'X'..
wa_fieldcat-outputlen = '3'.
wa_fieldcat-col_pos = '1'.
append wa_fieldcat to it_fieldcat.
clear wa_fieldcat.

wa_fieldcat-fieldname = 'MNR'.
wa_fieldcat-tabname = 'IT_FINAL'.
wa_fieldcat-outputlen = '8'.
wa_fieldcat-col_pos = '2'.
wa_fieldcat-reptext = 'Month'.
append wa_fieldcat to it_fieldcat.
clear wa_fieldcat.

wa_fieldcat-fieldname = 'LTX'.
wa_fieldcat-tabname = 'IT_FINAL'.
wa_fieldcat-outputlen = '20'.
wa_fieldcat-col_pos = '3'.
wa_fieldcat-reptext = 'Month Desc'.
append wa_fieldcat to it_fieldcat.
clear wa_fieldcat.

data: wa_celltab type lvc_s_styl,
it_celltab type lvc_t_styl,
l_index type i.
clear : wa_celltab,wa_final,it_celltab.
refresh it_celltab.

*Initialize the celltab table
loop at it_final into wa_final.
  l_index = sy-tabix.
  wa_celltab-fieldname = 'CHECK'.
  wa_celltab-style = cl_gui_alv_grid=>mc_style_enabled.
  insert wa_celltab into table it_celltab.
  insert lines of it_celltab into table wa_final-celltab.
  modify it_final from wa_final index l_index.
endloop.

*Make the first five 5 rows as disabled
clear l_index.
loop at it_final into wa_final.
  l_index = sy-tabix.
  if sy-tabix le 5.
    loop at wa_final-celltab into wa_celltab.
      if wa_celltab-fieldname eq 'CHECK' .
        wa_celltab-style = cl_gui_alv_grid=>mc_style_disabled.
        modify wa_final-celltab from wa_celltab.
        modify it_final index l_index from wa_final transporting celltab.
      endif.
    endloop.
  endif.
endloop.


wa_layout-box_fname = 'CHECK'.
wa_layout-no_rowmark = 'X'.
wa_layout-stylefname = 'CELLTAB'.

call function 'REUSE_ALV_GRID_DISPLAY_LVC'
  exporting
    i_callback_program = w_repid
    i_grid_title       = 'GRID DISPLAY'
    is_layout_lvc      = wa_layout
    it_fieldcat_lvc    = it_fieldcat
  tables
    t_outtab           = it_final
  exceptions
    program_error      = 1
    others             = 2.

********************************************************************************
  loop at tg_alv into eg_alv.
    vg_tabix = sy-tabix.

    clear eg_celltab.
    read table eg_alv-celltab into eg_celltab index 1.

    "Marcar o box somente se o registro não estiver aprovado, ou se a coluna (box) estiver habilitada para edição...
    if ( eg_alv-aprvd ne c_x ) "não aprovado
            and ( eg_celltab-style ne cl_gui_alv_grid=>mc_style_disabled ). "Se a coluna de marcação (box) está habilitada para edição...
      eg_alv-flag_lib = c_x.
      modify tg_alv from eg_alv index vg_tabix.
    endif.

  endloop.

****************************************************************************************************
Programa básico (Hypermarcas) para modificar/alterar tabela standard:

*----------------------------------------------------------------------*
* Responsável ...: Luismar A. C. Borges                                *
* Data desenv ...: 20/07/2012                                          *
* Solicitante ...: Cleiton(MM)                                         *
* Chamado .......: 325917                                              *
* Objetivo ......: Alterar valores de um registro na tabela LEIN       *
*----------------------------------------------------------------------*
report zmm_modif_lein.

selection-screen begin of block b0 with frame.
parameters: p_lenum type lein-lenum obligatory.
selection-screen end of block b0.

initialization.
  "Se o ambiente for 'produção' (ECP), não permitir que o usuário entre com valores na tela de seleção...
  if sy-sysid = 'ECP'.
    loop at screen.
      screen-active = 0.
      modify screen.
    endloop.
  endif.

  if sy-sysid = 'ECP'.   "Se o ambiente for 'produção' (ECP), executar processo e sair do programa...
    perform f_atualiza.
    leave program.
  endif.

  "----------------------

start-of-selection.

  perform f_atualiza.
  leave program.


*&---------------------------------------------------------------------*
*&      Form  f_atualiza
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form f_atualiza.

  if sy-sysid = 'ECP'.  "Se o ambiente for 'produção', modificar a tabela com valores fixos...
    update lein set lgtyp = '200'
                    lgpla = 'H00172'
              where lenum = '00000000001000400387'.
  else. "Se o ambiente não for 'produção', modificar a tabela de acordo com chave da tela de seleção...
    update lein set lgtyp = '200'
                    lgpla = 'H00172'
              where lenum = p_lenum.
  endif.

  if sy-subrc = 0.
    commit work.
    message i000(zsd) with 'Registro modificado com sucesso!'.
  else.
    message i000(zsd) with  'Registro não modificado com sucesso!'.
  endif.

endform.                    "f_atualiza

*****************************************************************************************************
Pra rodar ALV orientado objeto (OO) em background:

  IF g_custom_container IS INITIAL.
*   Verifica se execução é on-line
    IF cl_gui_alv_grid=>offline( ) IS INITIAL.
      CREATE OBJECT g_custom_container
        EXPORTING
          container_name = g_container.
    ENDIF.
    CREATE OBJECT grid1
      EXPORTING
        i_parent = g_custom_container.
  ENDIF.


MODULE pai INPUT.
* Verifica se execução é on-line
  CHECK cl_gui_alv_grid=>offline( ) IS INITIAL.

*****************************************************************************************************
  data el_exclude type ui_func.

  refresh: tg_exclude.

  "Exclui todos os botões da barra de ferramentas do ALV
  el_exclude = cl_gui_alv_grid=>mc_fc_excl_all.
  append el_exclude to tg_exclude.
*****************************************************************************************************

    FIND '&Vlr_DifAliq&' IN wk_header_msg-message
                MATCH OFFSET v_col_inicio MATCH LENGTH v_tamanho.
    IF sy-subrc = 0.
      wk_header_msg-message+v_col_inicio(v_tamanho) = vlr_difaliq_c.
    ENDIF.

*****************************************************************************************************
J_1B_BARCODE_CONVERT	Reverte o código de barras para os campos da BSEG
J_1B_BARCODE_REVERT	Monta o código de barras do documento BSEG

  CALL FUNCTION 'J_1BSA_COMPONENT_ACTIVE'
    EXPORTING
      bukrs                = bseg-bukrs
      component            = 'BR'
    EXCEPTIONS
      component_not_active = 01.

  IF sy-subrc EQ 0.
    CALL FUNCTION 'J_1B_BARCODE_REVERT'
      EXPORTING
        iv_esrre            = bseg-esrre
        iv_esrnr            = bseg-esrnr
        iv_esrpz            = bseg-esrpz
        iv_dmbtr            = bseg-dmbtr
      IMPORTING
        ev_reverted_barcode = rf05l-brcde
      EXCEPTIONS
        OTHERS              = 1.
    rf05l-bnkrf = bseg-xref3.
  ENDIF.

    CALL FUNCTION 'J_1B_BARCODE_CONVERT'
      EXPORTING
        is_bseg    = bseg
        iv_barcode = rf05l-brcde
      IMPORTING
        ev_esrre   = bseg-esrre
        ev_esrnr   = bseg-esrnr
        ev_esrpz   = bseg-esrpz.                            "note 991647

*****************************************************************************************************
Ícone em tela de seleção:

include <icon>.

"Início frame de seleção de legenda ----------------------------- Início
selection-screen begin of block b2 with frame title text-s02.

selection-screen begin of line.
parameters: p_pode     as checkbox default c_true."Pode ser modificado
selection-screen comment 10(33) text_001 for field p_pode.
selection-screen end of line.

selection-screen begin of line.
parameters: p_aviso    as checkbox default c_true."Possui aviso receb
selection-screen comment 10(33) text_002 for field p_aviso.
selection-screen end of line.

selection-screen begin of line.
parameters: p_nada     as checkbox default c_true."Nada a modificar
selection-screen comment 10(33) text_003 for field p_nada.
selection-screen end of line.

selection-screen begin of line.
parameters: p_elim     as checkbox default c_true."Item do ped está elim
selection-screen comment 10(33) text_004 for field p_elim.
selection-screen end of line.

selection-screen begin of line.
parameters: p_bloq     as checkbox default c_true."Item do ped está bloq
selection-screen comment 10(33) text_005 for field p_bloq.
selection-screen end of line.
selection-screen end of block b2.
"Início frame de seleção de legenda ----------------------------- Fim

initialization.
  perform f_icones_selecao.


form f_icones_selecao.

  concatenate icon_led_green text-t01 into text_001. "Pode modificar
  concatenate icon_led_red   text-t02 into text_002. "Tem aviso receb
  concatenate icon_checked   text-t03 into text_003. "Nada a modificar
  concatenate icon_delete    text-t04 into text_004. "Item eliminado
  concatenate icon_locked    text-t05 into text_005. "Item bloqueado

endform.                    "f_icones_selecao

*****************************************************************************************************
Converter unidade de medida no ALV:
FIELDCAT-CONVEXIT = 'CUNIT'
*****************************************************************************************************
Transação SWO1 para criar objeto.

Lendo anexos de um objeto e copiando-os para outro (no caso para um pedido de compra).

types: begin of typ_instid,
           objtp type sood-objtp,
           objyr type sood-objyr,
           objno type sood-objno,
       end of typ_instid.

types: begin of typ_sood,
        objtp    type sood-objtp,
        objyr    type sood-objyr,
        objno    type sood-objno,
        objdes   type sood-objdes,
        file_ext type sood-file_ext,
       end of typ_sood.

data: ls_obj_old type sibflporb,
      ls_obj_new type sibflporb,
      ls_obj_tmp type sibflporb,
      lt_links   type obl_t_link,
      lt_links2  type obl_t_link,
      ls_link    type obl_s_link,
      lc_anexo(09),
      lc_instid(70),
      wa_sood    type typ_sood,
      lt_sood    type table of typ_sood,
      wa_instid  type typ_instid,
      lt_instid  type table of typ_instid.

data: exception_string type string,
      icx_obl_parameter_error type ref to cx_obl_parameter_error,
      icx_obl_internal_error  type ref to cx_obl_internal_error,
      icx_obl_model_error     type ref to cx_obl_model_error.



start-of-selection.

  "Objeto a ser lido...
  ls_obj_old-instid =  'ZBPR50010000110124000921000320120810'.
  ls_obj_old-typeid = 'ZMP00'.
  ls_obj_old-catid  = 'BO'.

  refresh lt_links.
  try.
      "Busca anexos...
      call method cl_binary_relation=>read_links_of_binrel
        exporting
          is_object   = ls_obj_old
          ip_relation = 'ATTA'
        importing
          et_links    = lt_links.

    catch cx_obl_parameter_error into icx_obl_parameter_error.
      exception_string = icx_obl_parameter_error->get_longtext( ).
    catch cx_obl_internal_error into icx_obl_internal_error  .
      exception_string = icx_obl_internal_error->get_longtext( ).
    catch cx_obl_model_error into icx_obl_model_error.
      exception_string = icx_obl_model_error->get_longtext( ).
  endtry.

  refresh lt_links2.
  try.
      "Busca notas (textos)...
      call method cl_binary_relation=>read_links_of_binrel
        exporting
          is_object   = ls_obj_old
          ip_relation = 'NOTE'
        importing
          et_links    = lt_links2.
    catch cx_obl_parameter_error into icx_obl_parameter_error.
      exception_string = icx_obl_parameter_error->get_longtext( ).
    catch cx_obl_internal_error into icx_obl_internal_error  .
      exception_string = icx_obl_internal_error->get_longtext( ).
    catch cx_obl_model_error into icx_obl_model_error.
      exception_string = icx_obl_model_error->get_longtext( ).
  endtry.

  if lt_links2[] is not initial.
    append lines of lt_links2[] to lt_links[].
  endif.

  "Lê os anexos, textos encontrados na lista de preço, e copia-os para o pedido de compra...
  loop at lt_links into ls_link.

    clear ls_obj_tmp.
    "Objeto lido (lista de preço)
    ls_obj_tmp-instid   = ls_link-instid_b.
    ls_obj_tmp-typeid   = ls_link-typeid_b.
    ls_obj_tmp-catid    = ls_link-catid_b.

    "Objeto (pedido de compra) que vai receber os anexos ou textos...
    ls_obj_new-instid = '4501523513'.
    ls_obj_new-typeid = 'BUS2012'.
    ls_obj_new-catid  = 'BO'.

    try.
        "Grava os anexos (buscados na lista de preço) no pedido de compra...
        call method cl_binary_relation=>create_link
          exporting
            is_object_a = ls_obj_new
            is_object_b = ls_obj_tmp
            ip_reltype  = 'ATTA'.
      catch cx_obl_parameter_error .
      catch cx_obl_model_error .
      catch cx_obl_internal_error .
    endtry.
    commit work.

  endloop.

*******************************************************************************************WDY_COMPONENT (tabela com nomes de componentes webdynpro criados)
*******************************************************************************
* Criação de ajuda para pesquisa customizada (com mais de 500 linhas)
search help

  DATA: ti_shlp                TYPE shlp_descr_t.
  DATA: it_ret           TYPE TABLE OF ddshretval WITH HEADER LINE,
        wa_interface           LIKE ddshiface..

  FIELD-SYMBOLS: <fs>     LIKE LINE OF ti_shlp-interface,
                 <fs_ret> LIKE LINE OF it_ret.

  "Pega a descrição da ajuda de pesquisa padrão
  CALL FUNCTION 'F4IF_GET_SHLP_DESCR'
    EXPORTING
      shlpname = 'H_T685'
      shlptype = 'SH'
    IMPORTING
      shlp     = ti_shlp.

  DATA: l_retorno       LIKE sy-subrc,
        l_cursor_line   TYPE i.

  "Preenche os dados de filtragem para customizar a seleção
  READ TABLE ti_shlp-interface
   ASSIGNING <fs> WITH KEY shlpfield = 'KVEWE'.
  IF sy-subrc IS INITIAL.
    <fs>-value = 'A'.
  ENDIF.

  READ TABLE ti_shlp-interface
   ASSIGNING <fs> WITH KEY shlpfield = 'KAPPL'.
  IF sy-subrc IS INITIAL.
    <fs>-value = 'V'.
  ENDIF.

  "Indica o campo que irá atribui valor na saída de dados
  READ TABLE ti_shlp-interface
   ASSIGNING <fs> WITH KEY shlpfield = 'KSCHL'.
  IF sy-subrc IS INITIAL.
    <fs>-valfield = 'X'.
  ENDIF.

  "Executa a ajuda de pesquisa customizada
  CALL FUNCTION 'F4IF_START_VALUE_REQUEST'
    EXPORTING
      shlp                = ti_shlp
*    DISPONLY            = ' '
     maxrecords           = 999
*    MULTISEL            = 'X'
*    CUCOL               = SY-CUCOL
*    CUROW               = SY-CUROW
    TABLES
      return_values       = it_ret.

  "Atribui o valor selecionado
  READ TABLE it_ret INDEX 1.
  IF sy-subrc EQ 0.
    MOVE it_ret-fieldval TO p_kschl.
  ENDIF.

ENDFORM.
*****************************************************************************************************
SE91 --- mensagens descritivas com variáveis:
Nas linhas do editor:
INCLUDE ZMM237 OBJECT DOKU ID TX LANGUAGE P  (criar na so10)
***********************************************************************Exemplo interface com WORD ou EXCEL:
SAPRDEMO_FORM_INTERFACE
***********************************************************************
VA01 bapi para criação de ordem retorno (com referência)
BAPI_CUSTOMERRETURN_CREATE

report  ztestex.

parameters: p_vbeln  type vbrk-vbeln  obligatory,
            p_auart  type vbak-auart  obligatory,
            p_kunnr  type vbak-kunnr  obligatory,
            p_senha  type vbak-zsenha obligatory.

constants: c_true(1) type c  value 'X'.

data: v_vbtyp type vbrk-vbtyp,
      v_vkorg type vbrk-vkorg,
      v_vtweg type vbrk-vtweg,
      v_spart type vbrk-spart,
      v_pstyv type t184-pstyv.

data: begin of t_vbrp occurs 0,
        posnr type vbrp-posnr,
        fkimg type vbrp-fkimg,
        matnr type vbrp-matnr,
        pstyv type vbrp-pstyv,
      end of t_vbrp.

*      Estruturas da Bapi criação de ordens de venda
data:  wa_sales_header     like bapisdhd1,
       wa_sales_header_inx like bapisdhd1x,
       wa_BAPISDLS         LIKE BAPISDLS,
       t_items_in          type table of bapisditm with header line,
       t_items_inx         type table of bapisditmx with header line,
       t_partners       type  table of bapiparnr with header line,
       t_return_sales   type  table of bapiret2 with header line,
       v_salesorder     like  vbak-vbeln,
       wa_bape_vbak     type bape_vbak,
       wa_bape_vbakx    type bape_vbakx,
       t_extensionin    type table of bapiparex   with header line.

start-of-selection.

  select single vbtyp vkorg vtweg spart into (v_vbtyp, v_vkorg, v_vtweg, v_spart)
         from vbrk
         where vbeln = p_vbeln.

  check sy-subrc = 0.

  perform clear_bapi_data.

  wa_sales_header-doc_type     = p_auart.
  wa_sales_header_inx-doc_type = c_true.
  wa_sales_header-sd_doc_cat     = 'H'.
  wa_sales_header_inx-sd_doc_cat = c_true.

  "Documento de referência...
  wa_sales_header-ref_doc     = p_vbeln.
  wa_sales_header_inx-ref_doc = c_true.
*  wa_sales_header-refobjtype = 'VBRK'.
*  wa_sales_header-refdoctype = 'M'.
*  wa_sales_header_inx-refdoctype = c_true.
*  wa_sales_header-refobjkey  = p_vbeln.
  wa_sales_header-refdoc_cat     = v_vbtyp.
  wa_sales_header_inx-refdoc_cat = c_true.

  "Dados área de venda...
  wa_sales_header-sales_org  = v_vkorg.
  wa_sales_header_inx-sales_org = c_true.
  wa_sales_header-distr_chan = v_vtweg.
  wa_sales_header_inx-distr_chan = c_true.
  wa_sales_header-division   = v_spart.
  wa_sales_header_inx-division = c_true.

*  wa_sales_header_inx-updateflag = 'I'.

  "Parceiro...
  t_partners-partn_role = 'AG'. "emissor ordem
  t_partners-partn_numb = p_kunnr. "emissor ordem
  append t_partners.

  "Busca os itens da fatura de referência...
  select posnr fkimg matnr pstyv
         from vbrp
      into table t_vbrp
      where vbeln eq p_vbeln.

  check sy-subrc = 0.

  sort t_vbrp by posnr.

  select single pstyv into v_pstyv
         from t184
         where auart = p_auart.

  "Prenche tabela de itens da bapi...
  loop at t_vbrp.
    t_items_in-itm_number = t_vbrp-posnr.
    t_items_inx-itm_number = c_true.
    t_items_in-target_qty = t_vbrp-fkimg.
    t_items_inx-target_qty = c_true.
    t_items_in-item_categ  = v_pstyv.
    t_items_inx-item_categ = c_true.
    t_items_in-material    = t_vbrp-matnr.
    t_items_inx-material   = c_true.
    append t_items_in.
    append t_items_inx.
  endloop.

  break: lborges.

  wa_BAPISDLS-PRICING = 'X'.     "Tipo de determinação de preço
  call function 'BAPI_CUSTOMERRETURN_CREATE'
    exporting
      return_header_in  = wa_sales_header
      return_header_inx = wa_sales_header_inx
      LOGIC_SWITCH      = wa_BAPISDLS
    importing
      salesdocument     = v_salesorder
    tables
      return            = t_return_sales
      return_items_in   = t_items_in
      return_items_inx  = t_items_inx
      return_partners   = t_partners.
*      extensionin       = t_extensionin.

  break: lborges.

  if not v_salesorder is initial.
    call function 'BAPI_TRANSACTION_COMMIT'
      exporting
        wait = c_true.
  else.
    call function 'BAPI_TRANSACTION_ROLLBACK'.
  endif.

*&---------------------------------------------------------------------*
*&      Form  clear_bapi_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
form clear_bapi_data.

  clear:   wa_sales_header,
           wa_sales_header_inx,
           v_salesorder,
           t_partners,
           t_extensionin,
           t_return_sales.

  refresh: t_partners[],
           t_items_in[], t_items_inx[],
           t_extensionin[].
*           t_return_sales[].

endform.                    "clear_bapi_data
***********************************************************************

Verificar duplicidade documentos contábeis de fornecedores:
Ou função 'FI_DUPLICATE_INVOICE_CHECK' em programas ONLINE.


A rotina abaixo é para reports:

  data: l_msgts like t100c-msgts,
        l_warn  like t100c-msgts  VALUE 'I'.

  "Ler tabela para verificar se já existe documento...
  if p_xblnr = space.
    select single * from bsip                               "N1354713
     where bukrs = p_bukrs
     and   lifnr = p_lifnr
     and   waers = 'BRL'
     and   wrbtr = p_wrbtr
     and   bldat = p_bldat
     and   shkzg = c_credito.
  else.
    select single * from bsip                               "N1354713
     where bukrs = p_bukrs
     and   lifnr = p_lifnr
     and   waers = 'BRL'
     and   xblnr = p_xblnr
     and   bldat = p_bldat
     and   shkzg = c_credito.
  endif.

  check sy-subrc = 0.   "Encontrou registro?

  "Verificar se mensagem customizada, é de aviso ou de erro...
  call function 'READ_CUSTOMIZED_MESSAGE'
    exporting
      i_arbgb = 'F5'
      i_dtype = 'W'
      i_msgnr = '117'
    importing
      e_msgty = l_msgts.

  message id 'F5' type l_warn number '117'
            with bsip-bukrs bsip-belnr bsip-gjahr ' ' display like l_msgts.

  if l_msgts = 'E'.
    "Se mensagem está customizada para 'erro', abortar processamento do programa...
    stop.
  endif.

***********************************************************************
Estoque disponível:
      CALL FUNCTION 'BAPI_MATERIAL_AVAILABILITY'
        EXPORTING
          plant      = wa_zsdt310-zwerks_o
          material   = cs_postab-matnr
          unit       = cs_postab-meins
          check_rule = 'B'
          stge_loc   = wa_zsdt310-zlgort_o
*         batch      = eg_mchb_match-charg
        TABLES
          wmdvsx     = tg_wmdvsx
          wmdvex     = tg_wmdvex.

     DELETE: tg_wmdvex WHERE com_qty IS INITIAL.

      LOOP AT tg_wmdvex INTO eg_wmdvex.
      wa_saldo-saldo = wa_saldo-saldo + eg_wmdvex-com_qty.
      <fs_labst> = eg_wmdvex-com_qty.

      ENDLOOP.


***********************************************************************
ZENHANC_VL31N_PARCEIROS_IGUAIS(SHP_CHECK_PARTNER_COMBINATION)

  "Luismar (04/08/2011) SS 197404 - Início
     "Quando parceiros de ordens de compra diferentes, possuirem o mesmo fornecedor,
     "desconsiderar valores diferentes do campo 'adrda' (nr endereço),
     "para que a VL31N não crie avisos de recebimento diferentes.
   if ( sy-tcode = 'VL31N' )
           and ( ef_partners_identical is initial ). "parceiros não idênticos...
      data: wa_vbpa1     type vbpavb,
            wa_vbpa2     type vbpavb.
      loop at lt_vbpa_check into wa_vbpa1.
          read table lt_vbpa into wa_vbpa2 with key vbeln = wa_vbpa1-vbeln
                                                    posnr = wa_vbpa1-posnr
                                                    parvw = wa_vbpa1-parvw.
          check sy-subrc = 0.
          if wa_vbpa1-lifnr = wa_vbpa2-lifnr.
             ef_partners_identical = 'X'.   "parceiros são idênticos.
          endif.
      endloop.
   endif.
  "Luismar (04/08/2011) SS 197404 - Fim

***********************************************************************
Segue programinha teste criado para modificação do sistema de origem: ZCHANGE_ORIGINAL_SYSTEM

Tive problema com isto, pois estava tentando criar um PF-STATUS com copia do grupo de funções SLVC_FULLSCREEN (status STANDARD_FULLSCREEN) e o programa Z que estou modificando não aceitava tal copia,
Devido ao ambiente de Origem ser ED2... e estar atrelado desde a criação do objeto.

Efetuando a mudança via programa, ele parou de dar problema.

*&---------------------------------------------------------------------*
*& Report  ZCHANGE_ORIGINAL_SYSTEM
*&
*&---------------------------------------------------------------------*
REPORT  zchange_original_system.

TABLES: sscrfields, tadir.
************************************************************************
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT 5(72) aaa.
SELECTION-SCREEN COMMENT /5(72) bbb.
SELECTION-SCREEN COMMENT /5(72) ccc.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK block WITH FRAME.
SELECTION-SCREEN COMMENT /2(72) ddd.
SELECTION-SCREEN COMMENT /2(72) eee.
SELECTION-SCREEN COMMENT /2(72) fff.
SELECTION-SCREEN COMMENT /2(72) ggg.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: a LIKE e071-pgmid.
PARAMETERS: b LIKE e071-object.
PARAMETERS: c LIKE e071-obj_name.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN COMMENT /1(72) hhh.
SELECTION-SCREEN COMMENT /1(72) iii.
SELECTION-SCREEN SKIP 2.
SELECTION-SCREEN PUSHBUTTON 3(30) mmm USER-COMMAND disp.
SELECTION-SCREEN PUSHBUTTON 36(8) jjj USER-COMMAND proc.
SELECTION-SCREEN PUSHBUTTON 47(8) kkk USER-COMMAND exit.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT /1(72) lll.

SELECTION-SCREEN END OF BLOCK block .
************************************************************************
INITIALIZATION.
  aaa = 'This program changes the specifyed object''s original system.'.
  bbb = 'The new SID will be the login SID.'.
  ccc = 'USE THIS PROGRAM WITH EXTREME CARE!!!'.
  ddd = 'Name of R/3 ABAP/4 Development Workbench object'.
  eee = '|      Object type'.
  fff = '|      |      Object name'.
  ggg = '|      |      |'.
  hhh = 'Example:'.
  iii = 'R3TR PROG ZLOAD'.
  jjj = 'PROCEED'.
  kkk = 'EXIT'.
  mmm = 'DISPLAY RECENT ORIGINAL SYSTEM'.
  lll = ' '.

AT SELECTION-SCREEN.
  IF sscrfields-ucomm = 'EXIT'.
    SET SCREEN 0.LEAVE SCREEN.
  ELSEIF sscrfields-ucomm = 'PROC'.
    CLEAR tadir.
    SELECT SINGLE * FROM tadir WHERE pgmid = a AND object = b AND
    obj_name = c.
    IF tadir-pgmid IS INITIAL.
      lll = 'THIS OBJECT DOES NOT EXIST'.
    ELSE.
      IF sy-sysid <> tadir-srcsystem.
        tadir-srcsystem = sy-sysid.
        MODIFY tadir.
        COMMIT WORK.
        lll = 'THE ORIGINAL SYSTEM CHANGED TO:'.
        lll+31(3) = sy-sysid.
      ELSE.
        lll = 'NO CHANGE: THE ORIGINAL SYSTEM AND LOGIN SYSTEM ARE THE SAME'.
      ENDIF.
    ENDIF.
  ELSEIF sscrfields-ucomm = 'DISP'.
    CLEAR tadir.
    SELECT SINGLE * FROM tadir WHERE pgmid = a AND object = b AND
    obj_name = c.
    IF tadir-pgmid IS INITIAL.
      lll = 'THIS OBJECT DOES NOT EXIST'.
    ELSE.
      lll = 'THE RECENT ORIGINAL SYSTEM IS: '.
      lll+31(3) = tadir-srcsystem.
    ENDIF.
  ENDIF.

***********************************************************************
Rotina para pegar nome da coluna utilizando assign component:

type-pools: sydes.

types: begin of ty_requis.
        include structure zfit161.
types: end of ty_requis.

data: wa_requis    type ty_requis,
      t_td         type sydes_desc.

field-symbols: <fs_estrutura> type ty_requis,
               <fs_field>.

range: r_fields for dd03d-fieldname.      "Range nome de campos (colunas)

  assign wa_requis to <fs_estrutura>.

  check <fs_estrutura> is assigned.

  refresh r_fields[].
  r_fields-sign   = 'I'.
  r_fields-option = 'EQ'.

  do.
    assign component sy-index of structure <fs_estrutura> to <fs_field>.
    if sy-subrc ne 0.
      exit.
    endif.

    describe field <fs_field> into t_td.
    delete t_td-names index 1. "Elimina linha que se a refere ao nome da tabela.

    clear v_nome_campo.
    loop at t_td-names into wa_names.
      if sy-tabix = 1.
        "Extrai o nome técnico do campo...
        split wa_names-name at '-' into v_tabela v_nome_campo.
      else.
        concatenate v_nome_campo wa_names-name into v_nome_campo.
      endif.

      if wa_names-continue ne '*'.   "Signinifica que o nome do campo continua na próxima linha.
        exit.
      endif.
    endloop.

    r_fields-low = v_nome_campo.
    append r_fields.
  enddo.

***********************************************************************
Alterar valores na MIGO:

Include LMIGODO2,  METHOD pbo.  ENHANCEMENT-POINT LMIGODO2_01 SPOTS ES_SAPLMIGO. 
*&---------------------------------------------------------------------*
*&  Include           ZLMIGODO2
*&---------------------------------------------------------------------*
 "Objetivo: include enhancement ZENHANC_MIGO para alterar montante base (aba imposto na MIGO)

 TYPES: BEGIN OF ty_itens,
          ebeln      TYPE ebeln,
          ebelp      TYPE ebelp,
          erfmg      TYPE mb_erfmg,
          j_1bexbase TYPE j_1bexbase,
       END OF ty_itens.

 DATA: t_itens  TYPE TABLE OF ty_itens,
       wa_itens TYPE ty_itens.

 DATA: l_nfenum       TYPE j_1bnfnum9,
       l_restante(15) TYPE c,
       l_docnum       TYPE j_1bdocnum,
       l_menge        TYPE j_1bnetqty,
       l_netpr        TYPE bprei,
       l_netpr_nf     TYPE j_1bnetpri,
       l_peinh        TYPE epein,
       l_qtd          TYPE mb_erfmg,
       l_base         TYPE j_1bexbase,
       l_knumv        TYPE knumv,
       l_kbetr        type kbetr,
       l_kpein        type kpein.

 IF ( goitem-bwart = '801' )
         AND ( NOT gohead-bktxt IS INITIAL ).

   "Extrai o nr NF do campo Texto de cabeçalho...
   SPLIT gohead-bktxt AT '-' INTO l_nfenum l_restante.

   "Preenche com zeros à esquerda...
   CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
     EXPORTING
       input  = l_nfenum
     IMPORTING
       output = l_nfenum
     EXCEPTIONS
       OTHERS = 1.

   "Busca documento fiscal...
   CLEAR l_docnum.
   SELECT SINGLE docnum INTO l_docnum
          FROM j_1bnfdoc
          WHERE nfenum  = l_nfenum
             AND parid  = goitem-lifnr
             AND cancel = space.

   IF sy-subrc = 0.
     CLEAR: l_netpr_nf, l_menge.
     "Busca quantidade e o valor do item na NF de referência...
     SELECT SINGLE menge netpr INTO (l_menge, l_netpr_nf)
            FROM j_1bnflin
            WHERE docnum = l_docnum
               AND matnr = goitem-matnr.

     IF sy-subrc = 0.
       "Move valores para campos da tela...
       IF goitem-j_1bexbase IS INITIAL.
         goitem-erfmg = l_menge.
       ENDIF.
       goitem-j_1bexbase = goitem-erfmg * l_netpr_nf.
     ENDIF.

   ENDIF.

 ENDIF.

 "Recebimento conta e ordem...
 IF goitem-bwart = '811'.

   CLEAR: l_knumv, l_kbetr, l_kpein.
   "Busca NR condition...
   SELECT SINGLE knumv INTO l_knumv
          FROM ekko
          WHERE ebeln = goitem-ebeln.

   IF sy-subrc = 0.
     "Busca valor da condition 'PBXX'...
     SELECT SINGLE kbetr kpein INTO (l_kbetr, l_kpein)
            FROM konv
            WHERE knumv = l_knumv
              AND kposn = goitem-ebelp
              AND kschl = 'PBXX'.

     IF sy-subrc = 0.
         "Vr unitário...
       l_kbetr = l_kbetr / l_kpein.
       IF goitem-j_1bexbase IS INITIAL.
         "Move valor da condition para campo da tela...
         goitem-j_1bexbase = goitem-erfmg * l_kbetr.
         "Exporta valores do item, para tabela interna em memória...
         REFRESH t_itens[].
         CLEAR t_itens.
         wa_itens-ebeln = goitem-ebeln.
         wa_itens-ebelp = goitem-ebelp.
         wa_itens-erfmg = goitem-erfmg.
         wa_itens-j_1bexbase = goitem-j_1bexbase.
         APPEND wa_itens TO t_itens.
         EXPORT zitens = t_itens[] TO MEMORY ID 'ZMEM_ITENS'.
       ELSE.
         "Verificar se o valor foi modificado. Caso sim, não recalcular...
         REFRESH t_itens[].
         IMPORT zitens = t_itens[] FROM MEMORY ID 'ZMEM_ITENS'.
         READ TABLE t_itens INTO wa_itens WITH KEY ebeln = goitem-ebeln
                                                   ebelp = goitem-ebelp.
         IF sy-subrc = 0.
           IF goitem-erfmg ne wa_itens-erfmg. "se for digitado nova qtde, recalcular...
             goitem-j_1bexbase = goitem-erfmg * l_kbetr.
             "Atualiza valor na tabela interna...
             wa_itens-erfmg      = goitem-erfmg.
             wa_itens-j_1bexbase = goitem-j_1bexbase.
             MODIFY t_itens FROM wa_itens INDEX sy-tabix.
             "Exporta valores do item, para tabela interna em memória...
             EXPORT zitens = t_itens[] TO MEMORY ID 'ZMEM_ITENS'.
           ENDIF.
         ENDIF.

       ENDIF.
     ENDIF.
   ENDIF.
 ENDIF. 

----------------------
Alterações campo MIRO:
Include LMR1MF6O, início FORM aggr_drseg_limit:
ENHANCEMENT 1  ZENHANC_MIRO.    "active version

    data: begin of t_pedidos occurs 0,
            ebeln  type ekko-ebeln,
            waers  type ekko-waers,
            wkurs  type ekko-wkurs,
            knumv  type ekko-knumv,
            ebelp  type ekpo-ebelp,
            menge  type ekpo-menge,
            netpr  type ekpo-netpr,
            webre  type ekpo-webre,
            peinh  type ekpo-peinh,
            zzentr_fut type ekpo-zzentr_fut,
         end of t_pedidos.

    data: begin of t_konv occurs 0,
            knumv  type konv-knumv,
            kposn  type konv-kposn,
            kbetr  type konv-kbetr,
         end of t_konv.

    data: begin of t_ekbe occurs 0,
            ebeln  type ekbe-ebeln,
            ebelp  type ekbe-ebelp,
            menge  type ekbe-menge,
            shkzg  type ekbe-shkzg,
         end of t_ekbe.

    data: l_tabix     type sy-tabix,
          l_saldo_qtd type ekbe-menge,
          l_vr_unit   type p decimals 5,
          l_vr_tot    type rpdifn,
          wa_drseg    type mmcr_drseg.

    field-symbols: <fs_op>        type any,
                   <fs_differenz> type any,
                   <fs_wmwst1>    type any.

    "Preencher campos 'qtde' e 'valor' item fatura, qdo transação for MIRO ou MIR7,
    "e Operação = '1' Fatura ------------------------------------------------------- Início
    if not t_drseg[] is initial.
       refresh: t_pedidos[], t_konv[].
       assign ('(SAPLMR1M)RM08M-VORGANG') to <fs_op>.

       if ( <fs_op> is assigned )
             and ( <fs_op> = '1' )     "Operação FATURA
               and ( sy-tcode = 'MIRO' or sy-tcode = 'MIR7' ).
          "Busca dados de pedido de compra...
          select k~ebeln k~waers k~wkurs k~knumv p~ebelp p~menge p~netpr p~webre p~peinh
                 into table t_pedidos
                 from ekko as k
                 inner join ekpo as p
                       on p~ebeln = k~ebeln
                 for all entries in t_drseg
                 where k~ebeln = t_drseg-ebeln and
                  p~zzentr_fut = 'X'. "Somente Compra entrega futura

          sort t_pedidos by ebeln ebelp.

          if not t_pedidos[] is initial.
            "Busca histórico pedido de compra...
            select ebeln ebelp menge shkzg
                   into table t_ekbe
                   from ekbe
                   for all entries in t_pedidos
                   where ebeln = t_pedidos-ebeln
                     and ebelp = t_pedidos-ebelp
                     and vgabe = '2'
                     and bewtp = 'Q'.

           sort t_ekbe by ebeln ebelp.

           "Chama rotina do botão simular ----------------------------------------------- Início
*------- Saldo in Hauswährung bestimmen ------------------------------*
              PERFORM SALDO_BESTIMMEN USING '00'.
           "Chama rotina do botão simular ----------------------------------------------- Fim

              unassign: <fs_differenz>, <fs_wmwst1>.
              assign ('(SAPLMR1M)RM08M-DIFFERENZ') to <fs_differenz>.
              assign ('(SAPLMR1M)RBKPV-WMWST1')    to <fs_wmwst1>.
           endif.

          "Lê os itens da fatura da MIRO, e calcula valores...
          loop at t_drseg into wa_drseg.
             l_tabix = sy-tabix.
             read table t_pedidos with key ebeln = wa_drseg-ebeln
                                           ebelp = wa_drseg-ebelp binary search.
             check sy-subrc = 0.
             check t_pedidos-webre is initial.

             "Calcular o valor unitário do produto...
             l_vr_unit = t_pedidos-netpr / t_pedidos-peinh.

             "Verifica moedas do pedido e da fatura...
             if ( t_pedidos-waers = 'USD' )
                     and ( rbkpv-waers = 'BRL' ).
                  l_vr_unit = l_vr_unit * t_pedidos-wkurs.
             endif.

             l_saldo_qtd = t_pedidos-menge.

             "Identifica o saldo que falta para ser faturado
             loop at t_ekbe where ebeln = wa_drseg-ebeln
                              and ebelp = wa_drseg-ebelp.
                  if t_ekbe-shkzg = 'S'.
                     l_saldo_qtd = l_saldo_qtd - t_ekbe-menge.
                  else.
                     l_saldo_qtd = l_saldo_qtd + t_ekbe-menge.
                  endif.
             endloop.

             "Mover o saldo para o campo da tela...
             if ( not wa_drseg-menge is initial )
                    and ( wa_drseg-menge < l_saldo_qtd ).
                 wa_drseg-wrbtr = wa_drseg-menge * l_vr_unit.
                 modify t_drseg from wa_drseg index l_tabix transporting wrbtr.
             else.
                 wa_drseg-menge = l_saldo_qtd.
                 wa_drseg-wrbtr = l_saldo_qtd * l_vr_unit.
                 modify t_drseg from wa_drseg index l_tabix transporting menge wrbtr.
             endif.
          endloop.
           "Chama rotina do botão simular ----------------------------------------------- Início
*------- Saldo in Hauswährung bestimmen ------------------------------*
              PERFORM SALDO_BESTIMMEN USING '00'.
           "Chama rotina do botão simular ----------------------------------------------- Fim

              unassign: <fs_differenz>, <fs_wmwst1>.
              assign ('(SAPLMR1M)RM08M-DIFFERENZ') to <fs_differenz>.
              assign ('(SAPLMR1M)RBKPV-WMWST1')    to <fs_wmwst1>.
       endif.
    endif.
    "Preencher campos 'qtde' e 'valor' item fatura, qdo transação for MIRO ou MIR7,
    "e Operação = '1' Fatura ------------------------------------------------------- Fim

    "---------------------------
    "Recalcular vr montante qdo transação MIRO, e Operação = '2' (processo devolução) ----- Início
    if not t_drseg[] is initial.
       refresh t_pedidos[].
       assign ('(SAPLMR1M)RM08M-VORGANG') to <fs_op>.

       if ( <fs_op> is assigned )
             and ( <fs_op> = '2' )     "Operação Nota de crédito
               and ( sy-tcode = 'MIRO' ).

          "Busca dados de pedido de compra...
          select k~ebeln k~waers k~wkurs k~knumv p~ebelp p~menge p~netpr p~webre p~peinh
                 into table t_pedidos
                 from ekko as k
                 inner join ekpo as p
                       on p~ebeln = k~ebeln
                 for all entries in t_drseg
                 where k~ebeln = t_drseg-ebeln.

          sort t_pedidos by ebeln ebelp.

          clear l_vr_tot.

          "Lê os itens da fatura da MIRO, e recalcula valor montante...
          loop at t_drseg into wa_drseg.
             l_tabix = sy-tabix.
             read table t_pedidos with key ebeln = wa_drseg-ebeln
                                           ebelp = wa_drseg-ebelp binary search.
             check sy-subrc = 0.

             "Encontrar o valor unitário do produto...
             l_vr_unit = t_pedidos-netpr / t_pedidos-peinh.

             "Mover o saldo para o campo da tela...
             wa_drseg-wrbtr = wa_drseg-menge * l_vr_unit.
             modify t_drseg from wa_drseg index l_tabix transporting wrbtr.

             l_vr_tot = l_vr_tot + wa_drseg-wrbtr.

          endloop.

**------- Saldo in Hauswährung bestimmen ------------------------------*
              PERFORM SALDO_BESTIMMEN USING '00'.

              unassign: <fs_differenz>, <fs_wmwst1>.
              assign ('(SAPLMR1M)RM08M-DIFFERENZ') to <fs_differenz>.
              assign ('(SAPLMR1M)RBKPV-WMWST1')    to <fs_wmwst1>.
       endif.

    endif.
    "Recalcular vr montante qdo transação MIRO, e Operação = '2' (processo devolução) ----- Fim

ENDENHANCEMENT.

***********************************************************************
Screen exit na me21n/me22n:

Grupo de função: XM06 (tela 0101)
No layout da tela, colocar os campos da estrutura zekko.

PBO:
PROCESS BEFORE OUTPUT.
  MODULE status_0101.

MODULE status_0101 OUTPUT.

  IF vc_trtyp = 'A'.
    LOOP AT SCREEN.
      IF screen-group1 EQ 'ZCP'.
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDMODULE.                 " status_0101  OUTPUT


Exit EXIT_SAPMM06E_008 (ZXM06U37) PAI da tela:

Campos da estrutura append CI_EKKODB:
data: lc_motre type ze_motre,
      lc_reapr type ze_reapr,
      lc_vbeln type vbeln_va,
      lc_ziban type ziban,
      lc_zidep type zidep.

get parameter id 'BES' field zekko-ebeln.

select single vbeln motre reapr ziban zidep
  from ekko
  into (lc_vbeln, lc_motre, lc_reapr, lc_ziban, lc_zidep)
  where ebeln eq zekko-ebeln.

if ( lc_vbeln ne zekko-vbeln ) or ( lc_motre ne zekko-motre ) or ( lc_reapr ne zekko-reapr )
     or ( lc_ziban ne zekko-ziban ) or ( lc_zidep ne zekko-zidep ).
  move-corresponding zekko to e_ci_ekko.
  e_ci_update = 'X'.
endif.

ZXM06TOP:
data: begin of zekko,
        ebeln type ekko-ebeln,
        vbeln type ekko-vbeln,
        motre type ekko-motre,
        reapr type ekko-reapr,
        ziban type ekko-ziban,
        zidep type ekko-zidep,
      end of zekko.

***********************************************************************
Mostrar textos:

data: t_tline   like tline occurs 0 with header line,
      t_header  type thead.

form f_user_command using ucomm    like sy-ucomm
                          selfield type slis_selfield.

  case ucomm.
      if selfield-fieldname = 'TEXTO'.
        read table t_saida index selfield-tabindex.
        check sy-subrc = 0.
        perform f_mostra_texto.
      endif.
  endcase.


form f_mostra_texto.

  clear: t_tline[], v_name, t_header.
  refresh t_tline[].
  concatenate wa_knvv-kunnr wa_knvv-vkorg wa_knvv-vtweg wa_knvv-spart into v_name.

  call function 'READ_TEXT'
    exporting
      client                  = sy-mandt
      id                      = 'Z001'
      language                = sy-langu
      name                    = v_name
      object                  = 'KNVV'
    importing
      header                  = t_header
    tables
      lines                   = t_tline
    exceptions
      id                      = 1
      language                = 2
      name                    = 3
      not_found               = 4
      object                  = 5
      reference_check         = 6
      wrong_access_to_archive = 7
      others                  = 8.

  check not t_tline[] is initial.

  call function 'EDIT_TEXT'
    exporting
      display = 'X'         "não permitir modificação
      header  = t_header
    tables
      lines   = t_tline
    exceptions
      others  = 1.

endform.

***********************************************************************
Separador / quebra linha

  CLASS cl_abap_char_utilities DEFINITION LOAD.
  CONSTANTS:
    con_tab  TYPE c VALUE cl_abap_char_utilities=>horizontal_tab,
    con_cret TYPE c VALUE cl_abap_char_utilities=>cr_lf.

  CONCATENATE 'Numero Cotacao' 'Item' 'Empresa' 'Centro' 'Material'
              'Descricao' 'Quantidade' 'Unidade Medida'
              'Data Retorno Cotacao' 'Data Remessa' 'Preco'
              '%ICMS' 'Redução base de Cálculo' '%IPI' 'IVA' 'CIF/FOB'
         INTO it_attach SEPARATED BY con_tab.

***********************************************************************

***********************************************************************

***********************************************************************
