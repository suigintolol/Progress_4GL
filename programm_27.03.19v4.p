DEF WORK-TABLE tt_cl-good
    FIELD cod_good LIKE katalog.cod_good.

DEF QUERY q_classf FOR classf.
DEF QUERY q_tt_cl-good FOR tt_cl-good, katalog.

/* DEF WIDGETS */
/*����� ����� ��������������*/
DEF BROWSE b_classf QUERY q_classf 
    DISP classf.NAME 
    WITH 20 DOWN TITLE "�������������".

/*����� �������� ��������������*/
DEF BROWSE b_katalog QUERY q_tt_cl-good
    DISP katalog.cod_good katalog.NAME katalog.artic katalog.cod_firm
    WITH 25 DOWN TITLE "�������". 

DEF VAR checkboxsort AS LOGICAL FORMAT "TRUE/FALSE" INITIAL FALSE
     LABEL "�� ��������"
     VIEW-AS TOGGLE-BOX SIZE 30 BY 1 NO-UNDO.

DEF VAR comsorted AS CHARACTER FORMAT "X(256)":U
    LABEL "����������� ��" 
    INITIAL "������������"
    VIEW-AS COMBO-BOX
    LIST-ITEMS "������������", "�������", "���"
    INNER-LINES 15    
    DROP-DOWN-LIST
    SIZE 30 BY 1 NO-UNDO.

DEF VAR filtr_name AS CHARACTER FORMAT "X(256)":U 
     LABEL "������������" 
     VIEW-AS FILL-IN
     SIZE 20 BY 1 NO-UNDO.

DEF VAR filtr_art AS CHARACTER FORMAT "X(256)":U 
     LABEL "�������" 
     VIEW-AS FILL-IN
     SIZE 20 BY 1 NO-UNDO.

DEF VAR filtr_post AS CHARACTER FORMAT "X(256)":U 
     LABEL "��� ����������"
     VIEW-AS FILL-IN
     SIZE 20 BY 1 NO-UNDO.

DEF BUTTON btn_exit LABEL "�����.".

/* DEF FRAME */
/*�������� ������� ��������������*/
DEF FRAME f_main
    b_classf HELP "'->' �����, '<-' �����, 'Enter' �������� ������, 'F4' �����."
    btn_exit HELP "'F4' �����."
    WITH 1 COLUMN SIDE-LABELS SIZE 82 BY 20
    CENTERED TITLE "������ ���������������.".

/*�������� ������� ��������*/
DEF FRAME f_katalog
    b_katalog HELP "F5 ������,F6 ����������,-> ��������, Delete �������, Esc �����."
    WITH 1 COLUMN SIDE-LABELS SIZE 125 BY 30
    CENTERED TITLE "�������� ��������.".

/*�������� ����� ��� ��������*/
DEF FRAME f_katalog_filtr SKIP(1)
    filtr_name HELP "Esc-��������� �������� ������, F7-�������� ������."
    filtr_art HELP "Esc-��������� �������� ������, F7-�������� ������."
    filtr_post HELP "Esc-��������� �������� ������, F7-�������� ������."
    WITH 1 COLUMN SIDE-LABELS SIZE 50 BY 7
    VIEW-AS DIALOG-BOX CENTERED TITLE "������".

/*�������� ���������� ��� ��������*/
DEF FRAME f_katalog_sort SKIP(1)
    comsorted HELP "Esc-���������, �������� ����������."
    checkboxsort HELP "Esc-���������, �������� ����������." 
    WITH 1 COLUMN SIDE-LABELS SIZE 50 BY 7
    VIEW-AS DIALOG-BOX CENTERED TITLE "����������".

/*�������� ������� ��������*/
DEF FRAME f_katalog_update
    katalog.NAME HELP "������� ���������, F4-�����."
    WITH 
    VIEW-AS DIALOG-BOX TITLE "�������� ��������.".

/*����� ��� �������� (�� ����������)*/
DEF FRAME f_katalog_load SKIP(1)
    WITH 1 COLUMN SIDE-LABELS SIZE 50 BY 7
    VIEW-AS DIALOG-BOX CENTERED TITLE "��������� �������.".

/* MAIN LOGIC */
ENABLE ALL WITH FRAME f_main.
OPEN QUERY q_classf FOR EACH classf WHERE classf.parent_id = 0.
comsorted:SCREEN-VALUE = "������������".
RUN Enter(0).
DISP WITH FRAME f_main.

b_katalog:HEIGHT-CHAR = FRAME f_katalog:HEIGHT-CHAR - 1.
b_katalog:WIDTH-CHAR = FRAME f_katalog:WIDTH-CHAR - 1.

/*������� �� ��������� ������� ��������������*/
ON RIGHT OF b_classf DO:
    b_classf:REFRESH().
    IF AVAIL classf THEN DO:
        RUN enter(classf.id) NO-ERROR.
    END.
    ELSE DO:
        DISP "�������� ������!".
        RETURN.
    END.
END.

/*������� �� ���������� ������� ��������������*/ 
ON LEFT OF b_classf DO:
    b_classf:REFRESH().
    IF AVAIL classf THEN DO:
        RUN backspase(classf.parent_id).
    END.
    ELSE DO:
        DISP "��������� �� ����� ����� ������!".
        RETURN.
    END. 
END.

/*�������� ��� ������ �� ���������� ���� � �� �������� �����*/ 
ON Enter OF b_classf DO:
    /*���� ��������*/
    RUN clear_tt_cl-good.
    b_classf:REFRESH().
    IF AVAIL classf THEN DO:
        RUN recursion_select_by_classf(classf.id).
    END.
    ELSE DO:
        DISP "��������� �� ����� ����� ������!".
        RETURN.
    END. 
    RUN print_katalog.
    SET b_katalog WITH FRAME f_katalog.
END.

/*���������� �������� ��� ��������� ����� �������*/
ON VALUE-CHANGED OF 
    comsorted,
    checkboxsort, 
    filtr_name ,
    filtr_art,
    filtr_post DO:
    RUN print_katalog.
END.

/*���������� �������� ��� ����� ������ ������ */
ON VALUE-CHANGED OF b_katalog DO:
   /* DEF VAR target_cod_good AS DECIMAL.
    target_cod_good = katalog.cod_good.
    RUN print_katalog.
        REPEAT:
            IF katalog.cod_good = target_cod_good THEN LEAVE.   
            ELSE b_katalog:SELECT-NEXT-ROW().
        END.*/
    b_katalog:REFRESH().
END.

/*����� ����� ������� � ��������*/
ON F5 OF b_katalog DO: 
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
    RUN print_katalog.
END.

/*����� ����� ���������� � ��������*/
ON F6 OF b_katalog DO:
    SET comsorted
        checkboxsort 
        WITH FRAME f_katalog_sort.
END.

/* ��������� ������ */
ON RIGHT OF b_katalog DO:
    GET CURRENT q_tt_cl-good EXCLUSIVE-LOCK NO-WAIT.
    IF NOT AVAIL katalog THEN DO: 
        b_katalog:DELETE-SELECTED-ROWS().
        MESSAGE "������ ��� ������� ������ �������������." VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    END.
    ELSE DO:
        IF LOCKED katalog THEN MESSAGE "��� ������ � ������ ������ ������������ ������ �������������." VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        ELSE DO:
            ENABLE ALL WITH FRAME f_katalog_update.
            UPDATE katalog.NAME WITH FRAME f_katalog_update VIEW-AS DIALOG-BOX.
        END.
    END.
    GET CURRENT q_tt_cl-good NO-LOCK.
    b_katalog:REFRESH().
END.

/* �������� ������ */
ON DEL OF b_katalog DO:
    GET CURRENT q_tt_cl-good EXCLUSIVE-LOCK NO-WAIT.
    IF NOT AVAIL katalog THEN DO:
        b_katalog:DELETE-SELECTED-ROWS().
        MESSAGE "������ ��� ������� ������ �������������." VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
    END.
    ELSE DO:
        IF LOCKED katalog THEN MESSAGE "��� ������ � ������ ������ ������������ ������ �������������." VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        ELSE DO:
            MESSAGE "�� ������������� ������ �������: " + katalog.NAME + " ? " VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "��������" UPDATE del_button AS LOGICAL.
            IF del_button THEN DO:   
                FOR EACH cl-good EXCLUSIVE-LOCK OF katalog:
                    DELETE cl-good.
                END.
                FIND FIRST cl-good NO-LOCK.
                DELETE katalog.            
                b_katalog:DELETE-SELECTED-ROWS().
            END.
        END.
    END.
    b_katalog:REFRESH().
END.

/* ������� � ��������������  �� �����*/
ON LEFT OF b_katalog DO:
    /* ���-��, ��� ����� �������� */
END.

/* ����� �� ��������� */
ON F4 OF b_classf, btn_exit DO:     
    MESSAGE "�� ������������� ������ �����" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "�����." UPDATE exit_button AS LOGICAL.
    IF exit_button THEN DO: 
        QUIT.
    END.
END.

/* ����� �� ��������� */
ON CHOOSE OF btn_exit 
DO:
    MESSAGE "�� ������������� ������ �����" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "�����." UPDATE exit_button AS LOGICAL.
    IF exit_button THEN DO: 
        QUIT.
    END.
END.

/*����� �� ��������� */
ON Esc OF b_classf, btn_exit DO:     
    MESSAGE "�� ������������� ������ �����" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "�����." UPDATE exit_button AS LOGICAL.
    IF exit_button THEN DO: 
        QUIT.
    END.
END.

WAIT-FOR CHOOSE OF  btn_exit.

/***************************  PROCEDURE  ************************************/
PROCEDURE enter: 
    DEF INPUT PARAMETER target_id_classf AS INTEGER.
    FIND FIRST classf NO-LOCK WHERE classf.parent_id = target_id_classf NO-ERROR.
    IF AVAIL classf THEN DO:
        OPEN QUERY q_classf FOR EACH classf NO-LOCK WHERE classf.parent_id = target_id_classf AND classf.parent_id <> classf.id.
        FIND FIRST classf NO-LOCK WHERE classf.id = target_id_classf.
        b_classf:TITLE IN FRAME f_main = classf.NAME.
    END.
    ELSE DO:
        RUN clear_tt_cl-good.
        b_classf:REFRESH().
        RUN recursion_select_by_classf(target_id_classf).
        RUN print_katalog.
        SET b_katalog WITH FRAME f_katalog.
        RETURN.
    END.
END.

PROCEDURE backspase: 
    DEF INPUT PARAMETER target_p_id_classf AS INTEGER.
    IF target_p_id_classf = 0 THEN RETURN.
    FIND FIRST classf NO-LOCK WHERE classf.id = target_p_id_classf NO-ERROR.
    IF AVAIL classf THEN DO:
        RUN enter(classf.parent_id).
        b_classf:REFRESH() IN FRAME f_main.
        REPEAT:
            IF classf.id = target_p_id_classf THEN LEAVE.   
            ELSE b_classf:SELECT-NEXT-ROW().
        END.
    END.
    ELSE DO:
        RETURN.
    END.
END.

/*��������� ��� ������ ������� �������� � ��������� ��������������� �� ��������� �������*/
PROCEDURE add_to_tt_cl-good: 
    DEF INPUT PARAMETER target_id_classf AS INTEGER.
    FOR EACH cl-good NO-LOCK WHERE cl-good.id = target_id_classf:
        CREATE tt_cl-good.
        tt_cl-good.cod_good = cl-good.cod_good.
    END.
END.

PROCEDURE recursion_select_by_classf: 
    DEF INPUT PARAMETER target_id_classf AS INTEGER.
    RUN add_to_tt_cl-good(target_id_classf).
    DEF BUFFER buf_classf FOR classf.
    FOR EACH buf_classf NO-LOCK WHERE buf_classf.parent_id = target_id_classf:
        RUN recursion_select_by_classf(buf_classf.id).
    END.
END.

PROCEDURE clear_tt_cl-good:
    FOR EACH tt_cl-good:
        DELETE tt_cl-good.
    END.
END PROCEDURE.

/*��������� ������� ������ �� ��������� ������� �������� � browse �������� �� ��� ���������� � ������*/
PROCEDURE print_katalog:
    IF checkboxsort:SCREEN-VALUE IN FRAME f_katalog_sort = "TRUE" THEN DO:
        CASE comsorted:SCREEN-VALUE :
        WHEN "������������" THEN
            OPEN QUERY q_tt_cl-good FOR EACH tt_cl-good NO-LOCK, EACH katalog NO-LOCK WHERE tt_cl-good.cod_good = katalog.cod_good AND
            katalog.NAME MATCHES "*" + filtr_name:SCREEN-VALUE IN FRAME f_katalog_filtr + "*" AND katalog.artic MATCHES "*" + filtr_art:SCREEN-VALUE + "*" AND STRING(katalog.cod_firm) MATCHES "*" + filtr_post:SCREEN-VALUE + "*" BY katalog.NAME Descending.
        WHEN "�������" THEN
            OPEN QUERY q_tt_cl-good FOR EACH tt_cl-good NO-LOCK, EACH katalog NO-LOCK WHERE tt_cl-good.cod_good = katalog.cod_good AND
            katalog.NAME MATCHES "*" + filtr_name:SCREEN-VALUE + "*" AND katalog.artic MATCHES "*" + filtr_art:SCREEN-VALUE + "*" AND STRING(katalog.cod_firm) MATCHES "*" + filtr_post:SCREEN-VALUE + "*" BY katalog.artic Descending.
        WHEN "���" THEN
            OPEN QUERY q_tt_cl-good FOR EACH tt_cl-good NO-LOCK, EACH katalog NO-LOCK WHERE tt_cl-good.cod_good = katalog.cod_good AND
            katalog.NAME MATCHES "*" + filtr_name:SCREEN-VALUE + "*" AND katalog.artic MATCHES "*" + filtr_art:SCREEN-VALUE + "*" AND STRING(katalog.cod_firm) MATCHES "*" + filtr_post:SCREEN-VALUE + "*" BY katalog.cod_good Descending.
        END CASE.
    END.
    ELSE DO:
        CASE comsorted:SCREEN-VALUE IN FRAME f_katalog_sort:
        WHEN "������������" THEN
            OPEN QUERY q_tt_cl-good FOR EACH tt_cl-good NO-LOCK, EACH katalog NO-LOCK WHERE tt_cl-good.cod_good = katalog.cod_good AND
            katalog.NAME MATCHES "*" + filtr_name:SCREEN-VALUE + "*" AND katalog.artic MATCHES "*" + filtr_art:SCREEN-VALUE + "*" AND STRING(katalog.cod_firm) MATCHES "*" + filtr_post:SCREEN-VALUE + "*" BY katalog.name.
        WHEN "�������" THEN
            OPEN QUERY q_tt_cl-good FOR EACH tt_cl-good NO-LOCK, EACH katalog NO-LOCK WHERE tt_cl-good.cod_good = katalog.cod_good AND
            katalog.NAME MATCHES "*" + filtr_name:SCREEN-VALUE + "*" AND katalog.artic MATCHES "*" + filtr_art:SCREEN-VALUE + "*" AND STRING(katalog.cod_firm) MATCHES "*" + filtr_post:SCREEN-VALUE + "*" BY katalog.artic.
        WHEN "���" THEN
            OPEN QUERY q_tt_cl-good FOR EACH tt_cl-good NO-LOCK, EACH katalog NO-LOCK WHERE tt_cl-good.cod_good = katalog.cod_good AND
            katalog.NAME MATCHES "*" + filtr_name:SCREEN-VALUE + "*" AND katalog.artic MATCHES "*" + filtr_art:SCREEN-VALUE + "*" AND STRING(katalog.cod_firm) MATCHES "*" + filtr_post:SCREEN-VALUE + "*" BY katalog.cod_good.
        END CASE.
    END.
    RETURN.
END PROCEDURE.
