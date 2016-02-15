  IF ( T_TO_LIST_PLUS-PQUIT EQ 'X' or
       T_TO_LIST_PLUS-Pvqui EQ 'X' )
    AND ( T_TO_LIST_PLUS-VORGA NE 'ST' )
    AND ( T_TO_LIST_PLUS-VORGA NE 'SL' )
    AND ( T_TO_LIST_PLUS-vSOLA NE hlp_menga ).
*........mit Differenz quittierte Position.............................
    MOVE ICON_NOT_EQUAL_RED TO T_TO_LIST_PLUS-QSTAT.
  ELSEIF ( T_TO_LIST_PLUS-PQUIT EQ 'X' or
           T_TO_LIST_PLUS-Pvqui EQ 'X' )
    AND ( T_TO_LIST_PLUS-vsola EQ hlp_menga ).
*........ohne Differenz quittierte Position............................
    MOVE ICON_EQUAL_GREEN TO T_TO_LIST_PLUS-QSTAT.
  ELSEIF ( T_TO_LIST_PLUS-PQUIT EQ 'X'  or
           T_TO_LIST_PLUS-Pvqui EQ 'X' )
    AND ( ( T_TO_LIST_PLUS-VORGA EQ 'ST' )
    OR ( T_TO_LIST_PLUS-VORGA EQ 'SL' ) ).
*........stornierte Position...........................................
    MOVE ICON_DELETE TO T_TO_LIST_PLUS-QSTAT.
  ENDIF.
