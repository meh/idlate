# Copyleft (ɔ) meh. - http://meh.schizofreni.co
#
# This file is part of idlate - https://github.com/meh/idlate
#
# idlate is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of
# the License, or (at your option) any later version.
#
# idlate is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public
# License along with idlate. If not, see <http://www.gnu.org/licenses/>.

defmodule Idlate.Connection do
  defstruct client: nil,
            server: nil

  defmodule Client do
    defstruct [:ip, :host, :port, :secure]

    def load(connection, listener) do
      { ip, port } = connection |> Socket.remote!
      host         = case Socket.Host.by_address(ip) do
        { :ok, %Socket.Host{name: name} } ->
          name

        { :error, _ } ->
          Socket.Address.to_string(ip)
      end

      %Client{ip: ip, host: host, port: port, secure: Reagent.Listener.secure?(listener)}
    end
  end

  defmodule Server do
    defstruct [:ip, :host, :port, :secure]

    def load(listener) do
      { ip, port } = listener |> Socket.local!
      host         = case Socket.Host.by_address(ip) do
        { :ok, %Socket.Host{name: name} } ->
          name

        { :error, _ } ->
          Socket.Address.to_string(ip)
      end

      %Server{ip: ip, host: host, port: port, secure: Reagent.Listener.secure?(listener)}
    end
  end

  def load(connection) do
    %__MODULE__{
      client: Client.load(connection, connection |> Reagent.Connection.listener),
      server: Server.load(connection |> Reagent.Connection.listener)}
  end
end
