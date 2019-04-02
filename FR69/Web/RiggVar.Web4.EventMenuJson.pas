unit RiggVar.Web4.EventMenuJson;

(*
-     F                           
-    * * *                        
-   *   *   G                     
-  *     * *   *                  
- E - - - H - - - I               
-  *     * *         *            
-   *   *   *           *         
-    * *     *             *      
-     D-------A---------------B   
-              *                  
-              (C) federgraph.de  
*)

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Rest.Json.Types,
  REST.JsonReflect;

//https://flixengineering.com/archives/139

//https://stackoverflow.com/questions/48566890/how-to-hide-ownsobjects-and-listhelper-tobjectlists-properties-from-a-json

type
  SerializeObjectListAttribute = class(JsonReflectAttribute)
    constructor Create;
  end;

  TEventFolder = class
  public
    [JsonName('Folder')]
    FFolder: string;

    [JsonName('Items')]
    Items: array of string;
  end;

  TEventMenu = class
  public
    [JsonName('Path')]
    Path: string;

    [SerializeObjectList]
    [JsonName('Menu')]
    Menu: TList<TEventFolder>;

    constructor Create;
    destructor Destroy; override;
  end;

  TEventMenuJson = class
  public
    class function InitJsonN(ItemCount: Integer): string;
    class function InitJson(SL: TStrings): string;
  end;

implementation

uses
  Rtti,
  Rest.Json;

type
  TListOfObjectInterceptor = class(TJSONInterceptor)
    function ObjectsConverter(Data: TObject; Field: string): TListOfObjects; override;
  end;

{ TListOfObjectInterceptor }

function TListOfObjectInterceptor.ObjectsConverter(Data: TObject; Field: string): TListOfObjects;
var
  ctx: TRttiContext;
  list: TList<TObject>;
begin
  list := TList<TObject>(ctx.GetType(Data.ClassInfo).GetField(Field).GetValue(Data).AsObject);
  Result := TListOfObjects(list.List);
  SetLength(Result, list.Count);
end;

{ SerializeObjectListAttribute }

constructor SerializeObjectListAttribute.Create;
begin
  inherited Create(ctObjects, rtObjects, TListOfObjectInterceptor);
end;

{ TEventMenu }

constructor TEventMenu.Create;
begin
  Menu := TList<TEventFolder>.Create;
end;

destructor TEventMenu.Destroy;
begin
  Menu.Free;
  inherited;
end;

{ TEventMenuJson }

class function TEventMenuJson.InitJsonN(ItemCount: Integer): string;
var
  em: TEventMenu;
  ef: TEventFolder;
  i: Integer;
begin
  em := TEventMenu.Create;
  em.Path := 'angular/EventMenu';

  ef := TEventFolder.Create;
  em.Menu.Add(ef);

  ef.FFolder := 'Data';
  SetLength(ef.Items, ItemCount);
  for i := 1 to ItemCount do
  begin
    ef.Items[i-1] := IntToStr(i);
  end;

  result := TJson.ObjectToJsonString(em);

  em.Free;
  ef.Free;
end;

class function TEventMenuJson.InitJson(SL: TStrings): string;
var
  em: TEventMenu;
  ef: TEventFolder;
  i: Integer;
begin
  em := TEventMenu.Create;
  em.Path := 'angular/EventMenu';

  ef := TEventFolder.Create;
  em.Menu.Add(ef);

  ef.FFolder := 'Data';
  SetLength(ef.Items, SL.Count);
  for i := 0 to SL.Count-1 do
  begin
    ef.Items[i] := SL[i];
  end;

  result := TJson.ObjectToJsonString(em);

  em.Free;
  ef.Free;
end;

end.
