{$mode objfpc}
{$codepage UTF8}

unit btree;

interface

type
    PTree = ^BinTree;
    BinTree = record
        key : integer;
        left, right : PTree
    end;
    procedure insert_item(key: integer; var root : PTree); //Вставить элемент в дерево
    procedure print_tree(root : PTree); //Вывести элементы на экран в инфиксном порядке
    procedure delete_tree(var root: PTree); //Удалить все дерево
    function find_item(key: integer; root : PTree) : boolean;  //Найти элемент в дереве
    procedure delete_item(key: integer; var root: PTree);  //Удалить элемент из дерева

implementation

procedure insert_item(key: integer; var root : PTree);
begin
    if root = nil then
    begin
        new(root);
        root^.left := nil;
        root^.right := nil;
        root^.key := key
    end
    else
        if key < root^.key then
            insert_item(key, root^.left)
        else
            insert_item(key, root^.right);
end;

procedure print_tree(root : PTree);
begin
    if root = nil then
        WriteLn('Empty tree')
    else
    begin
        if root^.left <> nil then
            print_tree(root^.left);
        Writeln(root^.key);
        if root^.right <> nil then
            print_tree(root^.right);
    end;
end;

function find_item(key: integer; root : PTree) : boolean;
begin
    if root = nil then
        find_item := false
    else
        if root^.key = key then
            find_item := True
        else
            if key < root^.key then
                find_item := find_item(key, root^.left)
            else
                find_item := find_item(key, root^.right);
end;


// Удаляет минимальный элемент в правом поддереве, возвращает его ключ
function delete_min_item(var root: PTree): Integer;
var
    p: PTree ;
begin
    if root^.left = nil then
    begin
        p := root;
        delete_min_item := root^.key;
        root := root^.right;
        Dispose(p);
    end
    else
        delete_min_item := delete_min_item(root^.left);
end;

procedure delete_item(key: Integer; var root: PTree);
var
    p: PTree;
begin
    if root <> nil then
    begin
        // Ищем элемент--------------------------------------------
        if key < root^.key then
            delete_item(key,root^.left)
        else if key > root^.key then
            delete_item(key,root^.right)
                //---------------------------------------------------------
                // Если он является листом, просто удалим его--------------
        else if (root^.left = nil) and (root^.right = nil) then
        begin
            Dispose(root);
            root := nil;
        end
                //---------------------------------------------------------

                //Если у него есть только одно поддерево-------------------
        else if root^.left = nil then
        begin
            p := root;
            root := root^.right;
            Dispose(p);
        end
        else if root^.right = nil then
        begin
            p := root;
            root := root^.left;
            Dispose(p);
        end
                //---------------------------------------------------------

        else //Если есть оба поддерева, удаляем минимальный элемент
                //в правом поддереве, меняем ключи
            root^.key := delete_min_item(root^.right);
    end
    else
        WriteLn('Element not found');
end;


procedure delete_tree(var root: PTree);
begin
    if root <> nil then
    begin
        if root^.left <> nil then
            delete_tree(root^.left);
        if root^.right <> nil then
            delete_tree(root^.right);
        Dispose(root);
        root := nil;
    end;
end;

end.