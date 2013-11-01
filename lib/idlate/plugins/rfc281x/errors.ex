use Idlate.RFC281X.DSL

defnumeric Error.NoNicknameGiven, 431, [] do
  def to_string(_) do
    ":No nickname given"
  end
end

defnumeric Error.ErroneusNickname, 432, [:name] do
  def to_string(__MODULE__[name: name]) do
    "#{name} :Erroneus nickname"
  end
end

defnumeric Error.NicknameInUse, 433, [:name] do
  def to_string(__MODULE__[name: name]) do
    "#{name} :Nickname is already in use"
  end
end
