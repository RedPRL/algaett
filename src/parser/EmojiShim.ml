module Emoji = struct
  include Emoji
  let keycap_asterisk = "\x2a\xef\xb8\x8f\xe2\x83\xa3"
  let ladder = "\xf0\x9f\xaa\x9c"
  let all_emojis = keycap_asterisk :: ladder :: all_emojis
end
