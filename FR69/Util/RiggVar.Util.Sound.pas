unit RiggVar.Util.Sound;

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

const
  //Mapped Sounds
  Sound_MsgIn = 1;

  //Available Sounds
  Sound_Welcome = 0;
  Sound_Click01 = 1;
  Sound_Click02 = 2;
  Sound_Click03 = 3;
  Sound_Recycle = 4;

type
  TSoundManager = class
  private
    FEnabled: Boolean;
    FUseResource: Boolean;
    procedure SetEnabled(const Value: Boolean);
    function GetSoundFileName(SoundID: Integer): string;
    function GetResourceName(SoundID: Integer): string;
    procedure SetUseResource(const Value: Boolean);
  public
    procedure PlayWav(SoundID: Integer);
    property Enabled: Boolean read FEnabled write SetEnabled;
    property UseResource: Boolean read FUseResource write SetUseResource;
  end;

implementation

uses
  MMSystem;

procedure TSoundManager.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;

procedure TSoundManager.SetUseResource(const Value: Boolean);
begin
  FUseResource := Value;
end;

function TSoundManager.GetResourceName(SoundID: Integer): string;
var
  s: string;
begin
  result := '';
  case SoundID of
    0: s := 'welcome';
    1: s := 'click01';
    2: s := 'click02';
    3: s := 'click03';
    4: s := 'recycle';
  end;
  if (s <> '') then
    result := s;
end;

function TSoundManager.GetSoundFileName(SoundID: Integer): string;
var
  s: string;
begin
  result := '';
  s := GetResourceName(SoundID);
  if s <> '' then
    result := 'sound\' + s + '.wav';
end;

procedure TSoundManager.PlayWav(SoundID: Integer);
var
  s: string;
  fdwSound: Cardinal;
begin
  if Enabled then
  begin
    if UseResource then
    begin
      fdwSound := SND_RESOURCE or SND_ASYNC or SND_NOWAIT or SND_NODEFAULT;
      s := GetResourceName(SoundID);
      if s <> '' then
        PlaySound(PChar(s), HInstance, fdwSound);
    end
    else
    begin
      fdwSound := SND_ASYNC or SND_NOWAIT or SND_NODEFAULT;
      s := GetSoundFileName(SoundID);
      if s <> '' then
        PlaySound(PChar(s), 0, fdwSound);
    end;
  end;
end;

end.
