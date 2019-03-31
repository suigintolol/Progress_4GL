
/* DEFINE QUERY */
DEFINE TEMP-TABLE tt_katalog
    FIELD cod_good LIKE katalog.cod_good
    FIELD NAME LIKE katalog.NAME
    FIELD artic LIKE katalog.artic
    FIELD cod_firm LIKE katalog.cod_firm.
    
DEFINE QUERY q_classf FOR classf.
DEFINE QUERY q_tt_katalog FOR tt_katalog.

/* DEFINE WIDGETS */
/*����� ����� ��������������*/
DEFINE BROWSE b_classf QUERY q_classf 
    DISPLAY classf.id classf.NAME classf.PARENT_id
    WITH 7 DOWN TITLE "�������������".

/*����� �������� ��������������*/
DEFINE BROWSE b_tt_katalog QUERY q_tt_katalog 
    DISPLAY tt_katalog.cod_good tt_katalog.NAME tt_katalog.artic tt_katalog.cod_firm
    WITH 10 DOWN TITLE "�������". 

DEFINE VARIABLE checkboxsort AS LOGICAL FORMAT "TRUE/FALSE" INITIAL FALSE
     LABEL "�� ��������"
     VIEW-AS TOGGLE-BOX SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE var_classf_id AS INTEGER.
DEFINE VARIABLE var_back_classf_id AS INTEGER.

DEFINE VARIABLE comsorted AS CHARACTER FORMAT "X(256)":U
    LABEL "����������� ��" 
    INITIAL "������������"
    VIEW-AS COMBO-BOX
    LIST-ITEMS "������������", "�������", "���"
    INNER-LINES 15    
    DROP-DOWN-LIST
    SIZE 30 BY 1 NO-UNDO.

DEFINE VARIABLE filtr_name AS CHARACTER FORMAT "X(256)":U 
     LABEL "������������" 
     VIEW-AS FILL-IN
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE filtr_art AS CHARACTER FORMAT "X(256)":U 
     LABEL "�������" 
     VIEW-AS FILL-IN
     SIZE 20 BY 1 NO-UNDO.

DEFINE VARIABLE filtr_post AS CHARACTER FORMAT "X(256)":U 
     LABEL "��� ����������"
     VIEW-AS FILL-IN
     SIZE 20 BY 1 NO-UNDO.

DEFINE BUTTON exit_btn LABEL "�����.".

/* DEFINE FRAME */
/*�������� ������� �������������� � ��������*/
DEFINE FRAME f_main
    b_classf HELP "F5-�����, F6-�����, F7-�������� ��� ������ � �������� ����, F4-�����."
    b_tt_katalog HELP "F5-������, F6-����������, F7-�������������, F8-�������, F4-�����."
    exit_btn HELP "F4-�����."
    WITH 1 COLUMN SIDE-LABELS SIZE 132 BY 40
    CENTERED TITLE "�������� �������� �� ��������������.".

/*����� ��� �������*/
DEFINE FRAME f_katalog_filtr SKIP(1)
    filtr_name HELP "Esc-��������� �������� ������, F7-�������� ������."
    filtr_art HELP "Esc-��������� �������� ������, F7-�������� ������."
    filtr_post HELP "Esc-��������� �������� ������, F7-�������� ������."
    WITH 1 COLUMN SIDE-LABELS SIZE 50 BY 7
    VIEW-AS DIALOG-BOX CENTERED TITLE "������".

/*����� ��� ����������*/
DEFINE FRAME f_main_sort SKIP(1)
    comsorted HELP "Esc-���������, �������� ����������."
    checkboxsort HELP "Esc-���������, �������� ����������." 
    WITH 1 COLUMN SIDE-LABELS SIZE 50 BY 7
    VIEW-AS DIALOG-BOX CENTERED TITLE "����������".

/* MAIN LOGIC */
ENABLE ALL WITH FRAME f_main.
OPEN QUERY q_classf FOR EACH classf WHERE classf.PARENT_id = 0.
comsorted:SCREEN-VALUE = "������������".
RUN get_cl-good(0).
RUN print_tt_katalog.
DISPLAY WITH FRAME f_main.

/*����� �� �������������� � ��������*/
ON F4 OF b_classf, b_tt_katalog, exit_btn DO:     
    MESSAGE "�� ������������� ������ �����" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "�����." UPDATE exit_button AS LOGICAL.
    IF exit_button THEN DO: 
        QUIT.
    END.
END.

/*������� �� ��������� ������� ��������������*/
ON Enter OF b_classf DO:           
    IF NOT AVAIL classf THEN RETURN.
    var_classf_id = classf.id.
    RUN class_in_the_catalog.
END.

/*������� �� ���������� ������� ��������������*/ 
ON BACKSPACE OF b_classf DO:         
    IF var_classf_id = 0 THEN DO:
        RETURN.
    END.
    FIND FIRST classf WHERE classf.id = var_classf_id.
    var_back_classf_id = var_classf_id.
    var_classf_id = classf.parent_id.
    IF var_classf_id <> 0 THEN DO:
        FIND FIRST classf WHERE classf.id = var_classf_id.
    END.
    RUN class_in_the_catalog.

    REPEAT:
        IF classf.id = var_back_classf_id THEN LEAVE.   
        b_classf:SELECT-NEXT-ROW().
    END.

END.

/*�������� ��� ������ �� ���������� ���� � �� �������� �����*/ 
ON F7 OF b_classf DO:
    RUN clear_tt_katalog.
    RUN recursion_search_classf(classf.id).
    RUN print_tt_katalog.
END.

ON F9 OF b_classf DO:


OPEN QUERY q_tt_katalog FOR EACH tt_katalog WHERE cl-good.id = 97 AND
            tt_katalog.NAME MATCHES "*" + filtr_name:SCREEN-VALUE IN FRAME f_katalog_filtr + "*" AND tt_katalog.artic MATCHES "*" +
            filtr_art:SCREEN-VALUE + "*" AND STRING(tt_katalog.cod_firm) MATCHES "*" + 
            filtr_post:SCREEN-VALUE + "*" BY tt_katalog.cod_good Descending.


END.


/*���������� �������� ��� ��������� ����� �������*/
ON VALUE-CHANGED OF 
    comsorted,
    checkboxsort, 
    filtr_name ,
    filtr_art,
    filtr_post DO:
    RUN print_tt_katalog.
END.

/*���������� �������� ��� ����� ������*/
ON VALUE-CHANGED OF b_tt_katalog DO:
    b_tt_katalog:REFRESH().
END.

/*����� ����� ������� � ��������*/
ON F5 OF b_tt_katalog DO: 
    SET filtr_name
        filtr_art
        filtr_post 
        WITH FRAME f_katalog_filtr.
END.

/*������� ����� ����� �������*/
ON F7 OF
    filtr_name,
    filtr_art,
    filtr_post DO: 
    filtr_name:SCREEN-VALUE = "".
    filtr_art:SCREEN-VALUE = "".
    filtr_post:SCREEN-VALUE = "".
    RUN print_tt_katalog.
END.

/*����� ����� ���������� � ��������*/
ON F6 OF b_tt_katalog DO:
    SET comsorted
        checkboxsort 
        WITH FRAME f_main_sort.
END.

/*��������� ������������ ��������� ������ � ��������*/
ON F7 OF b_tt_katalog DO:
    FIND FIRST katalog EXCLUSIVE-LOCK WHERE tt_katalog.cod_good = katalog.cod_good NO-ERROR.
    IF NOT AVAIL katalog THEN DO:
        MESSAGE "������ ���� �������." VIEW-AS ALERT-BOX TITLE "��������!".
        b_tt_katalog:DELETE-SELECTED-ROWS().
        RETURN. 
    END.
    ELSE DO:
        tt_katalog.NAME = katalog.NAME.
                b_tt_katalog:REFRESH().
        UPDATE katalog.NAME HELP "Esc-������, Enter-��������� ���������." WITH VIEW-AS DIALOG-BOX TITLE "��������� ������".    
        tt_katalog.NAME = katalog.NAME.
        b_tt_katalog:REFRESH().
    END.
    FIND FIRST katalog NO-LOCK.
END.

/*�������� ��������� ������ � ��������*/
ON F8 OF b_tt_katalog DO:
    MESSAGE "�� ������������� ������ �������: " + tt_katalog.NAME + " ? " VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "��������" UPDATE del_button AS LOGICAL.
    IF del_button THEN DO: 
        FIND katalog EXCLUSIVE-LOCK WHERE tt_katalog.cod_good = katalog.cod_good NO-ERROR.
        IF NOT AVAIL katalog THEN DO:
            MESSAGE "������ ���� �������." VIEW-AS ALERT-BOX TITLE "��������!".
            b_tt_katalog:DELETE-SELECTED-ROWS().
            RETURN. 
        END.
        FOR EACH cl-good OF katalog:
            DELETE cl-good.
        END.
        DELETE katalog.
        b_tt_katalog:DELETE-SELECTED-ROWS().  
    END.
    ELSE DO:
        RETURN.
    END.
END.

/*
ON F8 OF b_tt_katalog DO:
    GET CURRENT qst EXCLUSIVE-LOCK NO-WAIT.
    IF LOCKED KATALOG THEN MESSAGE "STUDENT LOCKED"
        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    ELSE DO:
        ENABLE ALL WITH FRAME upd_frame.
        UPDATE KATALOG.name WITH FRAME upd_frame.
        DISPLAY KATALOG.NAME WITH BROWSE brws_st.
        GET CURRENT qst NO-LOCK.
    END.
END.*/


WAIT-FOR CHOOSE OF exit_btn.

/***************************  PROCEDURE  ************************************/

/*��������� ��� ������ ������� �������� � ��������� ��������������� �� ��������� �������*/
PROCEDURE print_kat:
    DEFINE INPUT PARAMETER id_cl-good AS decimal.
    FIND FIRST katalog NO-LOCK WHERE katalog.cod_good = id_cl-good NO-ERROR.
    IF AVAIL katalog THEN DO:    
        RUN add_from_kat NO-ERROR.
    END.
END.

/*������� ��� ������ � �������-������, ������� ����������� ���������� ��������������*/
PROCEDURE get_cl-good: 
    DEFINE INPUT PARAMETER id_classf AS INTEGER.
    FOR EACH cl-good NO-LOCK WHERE cl-good.id = id_classf:
        RUN print_kat(cl-good.cod_good).
    END.
END.

PROCEDURE get_cl-good2: 
    DEFINE INPUT PARAMETER id_classf AS INTEGER.
    FOR EACH cl-good WHERE cl-good.id = id_classf:
        FIND katalog OF cl-good NO-LOCK NO-ERROR.
        IF AVAIL katalog THEN DO:
            RUN add_from_kat.
        END.
    END.
END.

/*������� ��� ���� �������������� ������������� ���������� ��������������*/
PROCEDURE recursion_search_classf:
    DEFINE INPUT PARAMETER id_classf AS INTEGER.
    DEFINE BUFFER buf_classf FOR classf.
    RUN get_cl-good2(id_classf).
    FIND FIRST buf_classf.
    REPEAT:
        IF buf_classf.parent_id = id_classf THEN DO:
            RUN recursion_search_classf(buf_classf.id).
        END.
        FIND NEXT buf_classf.
    END.
END.

/*�������� ������ �������������� � ���������*/
PROCEDURE class_in_the_catalog:
    IF var_classf_id = 0 THEN DO:
        b_classf:TITLE IN FRAME f_main = "�������������".
    END.
    ELSE DO:
        b_classf:TITLE = classf.name.
    END.
    FIND FIRST classf WHERE var_classf_id = classf.parent_id NO-ERROR.
    IF AVAIL classf THEN DO:
        OPEN QUERY q_classf FOR EACH classf WHERE var_classf_id = classf.parent_id.
    END.
    RUN clear_tt_katalog.
    RUN get_cl-good2(var_classf_id).
    RUN print_tt_katalog.
END PROCEDURE.

/*������� ��������� �������*/
PROCEDURE clear_tt_katalog:
    FOR EACH tt_katalog:
        DELETE tt_katalog.
    END.
END PROCEDURE.

/*��������� ������� ������ �� ��������� ������� �������� � browse �������� �� ��� ���������� � ������*/
PROCEDURE print_tt_katalog:
    IF checkboxsort:SCREEN-VALUE IN FRAME f_main_sort = "TRUE" THEN DO:
        CASE comsorted:SCREEN-VALUE IN FRAME f_main_sort:
        WHEN "������������" THEN
            OPEN QUERY q_tt_katalog FOR EACH tt_katalog WHERE
            tt_katalog.NAME MATCHES "*" + filtr_name:SCREEN-VALUE IN FRAME f_katalog_filtr + "*" AND tt_katalog.artic MATCHES "*" + filtr_art:SCREEN-VALUE + "*" AND STRING(tt_katalog.cod_firm) MATCHES "*" + filtr_post:SCREEN-VALUE + "*" BY tt_katalog.NAME Descending.
        WHEN "�������" THEN
            OPEN QUERY q_tt_katalog FOR EACH tt_katalog WHERE
            tt_katalog.NAME MATCHES "*" + filtr_name:SCREEN-VALUE + "*" AND tt_katalog.artic MATCHES "*" + filtr_art:SCREEN-VALUE + "*" AND STRING(tt_katalog.cod_firm) MATCHES "*" + filtr_post:SCREEN-VALUE + "*" BY tt_katalog.artic Descending.
        WHEN "���" THEN
            OPEN QUERY q_tt_katalog FOR EACH tt_katalog WHERE
            tt_katalog.NAME MATCHES "*" + filtr_name:SCREEN-VALUE + "*" AND tt_katalog.artic MATCHES "*" + filtr_art:SCREEN-VALUE + "*" AND STRING(tt_katalog.cod_firm) MATCHES "*" + filtr_post:SCREEN-VALUE + "*" BY tt_katalog.cod_good Descending.
        END CASE.
    END.
    ELSE DO:
        CASE comsorted:SCREEN-VALUE IN FRAME f_main_sort:
        WHEN "������������" THEN
            OPEN QUERY q_tt_katalog FOR EACH tt_katalog WHERE
            tt_katalog.NAME MATCHES "*" + filtr_name:SCREEN-VALUE + "*" AND tt_katalog.artic MATCHES "*" + filtr_art:SCREEN-VALUE + "*" AND STRING(tt_katalog.cod_firm) MATCHES "*" + filtr_post:SCREEN-VALUE + "*" BY tt_katalog.name.
        WHEN "�������" THEN
            OPEN QUERY q_tt_katalog FOR EACH tt_katalog WHERE
            tt_katalog.NAME MATCHES "*" + filtr_name:SCREEN-VALUE + "*" AND tt_katalog.artic MATCHES "*" + filtr_art:SCREEN-VALUE + "*" AND STRING(tt_katalog.cod_firm) MATCHES "*" + filtr_post:SCREEN-VALUE + "*" BY tt_katalog.artic.
        WHEN "���" THEN
            OPEN QUERY q_tt_katalog FOR EACH tt_katalog WHERE
            tt_katalog.NAME MATCHES "*" + filtr_name:SCREEN-VALUE + "*" AND tt_katalog.artic MATCHES "*" + filtr_art:SCREEN-VALUE + "*" AND STRING(tt_katalog.cod_firm) MATCHES "*" + filtr_post:SCREEN-VALUE + "*" BY tt_katalog.cod_good.
        END CASE.
    END.
    RETURN.
END PROCEDURE.

/*�������� ��������� ������ �������� �� ��������� �������*/
PROCEDURE add_from_kat:
    CREATE tt_katalog.
    tt_katalog.cod_good = katalog.cod_good.
    tt_katalog.name = katalog.name.
    tt_katalog.cod_firm = katalog.cod_firm.
    tt_katalog.artic = katalog.artic.
END PROCEDURE.

