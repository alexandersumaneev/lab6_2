{$mode objfpc}
{$codepage UTF8}

unit cmd;

interface
uses btree,crt,regexpr;
var
    y_lstr, c_lstr,buf_size: integer;
    symbols: set Of Char;
    lstr: AnsiString;
    cmd_list: array[1..6] of ansistring;
    prev_cmd: array[1..200] of AnsiString;
    n_cmd, i_cmd: integer;
    my_tree: PTree;

    procedure init();

implementation

const
    number = '^(0|(-(1|2|3|4|5|6|7|8|9)\d*)|((1|2|3|4|5|6|7|8|9)\d*))$';

var
    i: integer;

procedure help();
var
    f: Text;
    s: string;
begin
    Assign(f,'help.txt');
    Reset(f);
    while not Eof(f) do
    begin
        ReadLn(f,s);
        WriteLn(s);
    end;
    Close(f);
end;

procedure esc();
begin
    delete_tree(my_tree);
    halt();
end;

procedure del_spaces(var cmd: ansistring);
begin
    cmd := ReplaceRegExpr('\s+',cmd,' ',false);
    cmd := ReplaceRegExpr('(^\s)|(\s$)',cmd,'',false);
end;

procedure delete_f(arg: string);
var
    i: Integer;
    s: string;
begin
    if not ExecRegExpr('(tree$)|' + number, arg) then
        WriteLn('Invalid parameter')
    else
        if arg = 'tree' then
            delete_tree(my_tree)
        else
        begin
            Val(arg,i);
            str(i,s);
            if s = arg then
                delete_item(i,my_tree)
            else
                WriteLn('Not an integer');
        end;
end;

procedure insert_f(arg: string);
var
    i: integer;
    s: string;
begin
    if not ExecRegExpr(number, arg) then
        WriteLn('Invalid parameter')
    else
    begin
        Val(arg,i);
        str(i,s);
        if s = arg then
            insert_item(i,my_tree)
        else
            WriteLn('Not an integer');
    end;
end;

procedure find_f(arg: string);
var
    i: integer;
    s: string;
begin
    if not ExecRegExpr(number, arg) then
        WriteLn('Invalid parameter')
    else
    begin
        Val(arg,i);
        str(i,s);
        if s = arg then
            WriteLn(find_item(i,my_tree))
        else
            WriteLn('Not an integer');
    end;
end;

procedure print_f(arg: string);
begin
    if arg <> '' then
        WriteLn('The print procedure has no parameters')
    else
        print_tree(my_tree);
end;

procedure help_f(arg: string);
begin
    if arg <> '' then
        WriteLn('The help procedure has no parameters')
    else
        help();
end;

procedure clear_f(arg: string);
begin
    if arg <> '' then
        WriteLn('The clearscr procedure has no parameters')
    else
    begin
        clrscr;
        lstr := '';
    end;
end;

procedure split();
var
    space_pos: Integer;
    cmd: ansistring; //Команда после разбиения строки
    cmd_arg: ansistring; //Аргумент команды

procedure cmd_exec();
begin
    if not ExecRegExpr('(clearscr|help|print|delete|insert|find)((\s.*)|$)',cmd) then
        writeln(#10#13,'Command   *',lstr,'*  not found')
    else
    begin
        if cmd = 'delete' then
            delete_f(cmd_arg);
        if cmd = 'insert' then
            insert_f(cmd_arg);
        if cmd = 'find' then
            find_f(cmd_arg);
        if cmd = 'print' then
            print_f(cmd_arg);
        if cmd = 'help' then
            help_f(cmd_arg);
        if cmd = 'clearscr' then
            clear_f(cmd_arg);
    end;
end;

begin
    WriteLn();
    space_pos := pos(' ',lstr);
    cmd_arg := '';
    if space_pos <> 0 then //Если нашли пробел разбиваем команду на 2 части
    begin
        cmd := Copy(lstr,1,space_pos - 1);
        cmd_arg := Copy(lstr,space_pos + 1,Length(lstr));
        cmd_exec();
    end
    else
    begin
        cmd := lstr;
        cmd_exec();
    end;
end;

procedure setxy();
begin
    gotoxy(1 + (c_lstr mod buf_size),y_lstr + (c_lstr div buf_size));
end;

procedure setcp();
begin
    c_lstr := buf_size * (wherey - y_lstr) + wherex;
end;

procedure dp();
begin
    for i:=0 to Length(lstr) div buf_size do
    begin
        gotoxy(1,y_lstr + i);
        delline;
    end;
    gotoxy(1,y_lstr);
    Write(lstr);
end;

procedure left();
begin
    if(c_lstr > 0) then
    begin
        c_lstr := c_lstr - 1;
        setxy();
    end;
end;

procedure right();
begin
    if(c_lstr < Length(lstr)) then
    begin
        c_lstr := c_lstr + 1;
        setxy();
    end;
end;

procedure up();
begin
    if i_cmd > 1 then
    begin
        i_cmd := i_cmd - 1;
        lstr := prev_cmd[i_cmd];
        dp();
        c_lstr := Length(lstr);
        setxy();
        clreol;
    end;
end;

procedure down();
begin
    if i_cmd < n_cmd then
    begin
        i_cmd := i_cmd + 1;
        lstr := prev_cmd[i_cmd];
        dp();
        c_lstr := Length(lstr);
        setxy();
        clreol;
    end;
end;

procedure enter();
begin
    n_cmd += 1;
    i_cmd := n_cmd + 1;
    prev_cmd[n_cmd] := lstr;
    del_spaces(lstr);
    split();
    lstr := '';
    y_lstr := wherey;
    c_lstr := 1;
end;

Procedure backspace();
Begin
    if c_lstr > 0 then
    begin
        delete(lstr,c_lstr,1);
        dp();
        left();
    end;
End;

procedure tab();
var
    s: ansistring;
begin
    s := lstr;
    del_spaces(s);
    for i:=1 to 6 do
    begin
        if pos(s,cmd_list[i]) = 1 then //Если нашли команду в списке команд
        begin
            lstr := cmd_list[i]+' ';
            dp();
            c_lstr := Length(lstr);
            setxy();
            clreol;
            break;
        end;
    end;
end;

procedure key_press();
var
    key: char;
begin
    key := readkey;
    if wherey > 23 then
    begin
        clrscr;
        y_lstr := 1;
        dp();
        setxy();
        backspace();
    end;
    If (key in symbols) Then
    Begin
        if length(lstr) < buf_size*2 then
        begin
            setcp();
            insert(key,lstr,c_lstr);
            dp();
            setxy();
        end;
    End;
    If (key = #27) Then esc;
    If (key = #13) Then enter;
    If (key = #9) then tab;
    If (key = #8) then backspace;
    If (key = #0) Then
        Case readkey() Of
        #72:up;
        #80:down;
        #75:left;
        #77:right;
        End;
end;

procedure init();
begin
    help();
    y_lstr := wherey;
    c_lstr := 1;
    while(true) do
        key_press();
end;

begin
    my_tree := nil;
    n_cmd := 0;
    buf_size:=80;
    prev_cmd[1] := '';
    cmd_list[1] := 'help';
    cmd_list[2] := 'insert';
    cmd_list[3] := 'print';
    cmd_list[4] := 'find';
    cmd_list[5] := 'delete';
    cmd_list[6] := 'clearscr';
    symbols := ['a'..'z','0' .. '9',' ','-'];
end.