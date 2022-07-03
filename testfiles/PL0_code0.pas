const a=10;
var d,e,f;
procedure p;
  var g;
  procedure q;
    begin
      g := 3;
      e := g
    end;
  begin
    d:=a*2;
    e:=a/3;
    if d<=e then f:=d+e;
    call q
  end;
begin
  read(e,f);
  write(e,f,d);
  call p;
  write (e);
  while odd d do e:=-e+1
end.