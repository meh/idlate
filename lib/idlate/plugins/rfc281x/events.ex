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

use Idlate.RFC281X.DSL

defevent Password, content: nil
defevent Nick, name: nil
defevent User, name: nil, real_name: nil, modes: nil

defevent Ping, cookie: nil
defevent Pong, cookie: nil

defevent Join, channel: nil, password: nil
defevent Part, channel: nil, reason: nil
defevent Message, from: nil, to: nil, content: nil
