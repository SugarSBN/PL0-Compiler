var x, y;
procedure prime;
    procedure plus;
        x := x + y;
    begin
        call plus
    end;
begin
    read (y);
    read (x);
    call prime;
    write (x)
end
        