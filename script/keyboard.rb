#Encoding: UTF-8
#This file is part of Xdo. 
#Copyright © 2009 Marvin Gülker
#  Initia in potestate nostra sunt, de eventu fortuna iudicat. 
require_relative("../xdo")

module XDo
  
  #A namespace encabsulating methods to simulate keyboard input. You can 
  #send input to special windows, use the +w_id+ parameter of many methods 
  #for that purpose. 
  #NOTE: xdotool seams to reject the <tt>--window</tt> option even if you try 
  #to run it directly. This command fails (where 60817411 is a window id): 
  #  xdotool key --window 60817411 a
  #So don't be surprised if it does not work with this library. Hopefully this will be 
  #fixed, so I leave this in. 
  module Keyboard
    
    #Für jede Escape-Sequenz eine Klasse mit Befehlen erzeugen
    [
      [:BS, ["#{XDOTOOL} key%s BackSpace"]], 
      [:BACKSPACE, ["#{XDOTOOL} key%s BackSpace"]], 
      [:DEL, ["#{XDOTOOL} key%s Delete"]], 
      [:UP, ["#{XDOTOOL} key%s Up"]], 
      [:DOWN, ["#{XDOTOOL} key%s Down"]], 
      [:RIGHT, ["#{XDOTOOL} key%s Right"]], 
      [:LEFT, ["#{XDOTOOL} key%s Left"]], 
      [:HOME, ["#{XDOTOOL} key%s Home"]], 
      [:END, ["#{XDOTOOL} key%s End"]], 
      [:ESC, ["#{XDOTOOL} key%s Escape"]], 
      [:INS, ["#{XDOTOOL} key%s Insert"]], 
      [:PGUP, ["#{XDOTOOL} key%s Prior"]], 
      [:PGDN, ["#{XDOTOOL} key%s Next", "#{XDOTOOL} key%s Next"]], 
      [:F1, ["#{XDOTOOL} key%s F1"]], 
      [:F2, ["#{XDOTOOL} key%s F2"]], 
      [:F3, ["#{XDOTOOL} key%s F3"]], 
      [:F4, ["#{XDOTOOL} key%s F4"]], 
      [:F5, ["#{XDOTOOL} key%s F5"]], 
      [:F6, ["#{XDOTOOL} key%s F6"]], 
      [:F7, ["#{XDOTOOL} key%s F7"]], 
      [:F8, ["#{XDOTOOL} key%s F8"]], 
      [:F9, ["#{XDOTOOL} key%s F9"]], 
      [:F10, ["#{XDOTOOL} key%s F10"]], 
      [:F11, ["#{XDOTOOL} key%s F11"]], 
      [:F12, ["#{XDOTOOL} key%s F12"]], 
      [:TAB, ["#{XDOTOOL} key%s Tab"]], 
      [:PRINT, ["#{XDOTOOL} key%s Print"]], 
      [:NUM_LOCK, ["#{XDOTOOL} key%s Num_Lock"]], 
      [:PAUSE, ["#{XDOTOOL} key%s Pause"]], 
      [:CAPS_LOCK, ["#{XDOTOOL} key%s Caps_Lock"]], 
      [:NUM1, ["#{XDOTOOL} key%s KP_End"]], 
      [:NUM2, ["#{XDOTOOL} key%s KP_Down"]], 
      [:NUM3, ["#{XDOTOOL} key%s KP_Next"]], 
      [:NUM4, ["#{XDOTOOL} key%s KP_Left"]], 
      [:NUM5, ["#{XDOTOOL} key%s KP_Begin"]], 
      [:NUM6, ["#{XDOTOOL} key%s KP_Right"]], 
      [:NUM7, ["#{XDOTOOL} key%s KP_Home"]], 
      [:NUM8, ["#{XDOTOOL} key%s KP_Up"]], 
      [:NUM9, ["#{XDOTOOL} key%s KP_Prior"]], 
      [:NUM_DIV, ["#{XDOTOOL} key%s KP_Divide"]], 
      [:NUM_MUL, ["#{XDOTOOL} key%s KP_Multiply"]], 
      [:NUM_SUB, ["#{XDOTOOL} key%s KP_Subtract"]], 
      [:NUM_ADD, ["#{XDOTOOL} key%s KP_Add"]], 
      [:NUM_ENTER, ["#{XDOTOOL} key%s KP_Enter"]], 
      [:NUM_DEL, ["#{XDOTOOL} key%s KP_Delete"]], 
      [:NUM_COMMA, ["#{XDOTOOL} key%s KP_Separator"]], 
      [:NUM_INS, ["#{XDOTOOL} key%s KP_Insert"]], 
      [:NUM0, ["#{XDOTOOL} key%s KP_0"]], 
      [:SCROLL_LOCK, ["#{XDOTOOL} key%s Scroll_Lock"]], 
      
      [:REV_TAB, ["#{XDOTOOL} key%s shift+Tab"]]
    ].each do |name, commands|
      $commands = commands
      XDo::Keyboard.const_set(name, Class.new do
        COMMANDS = $commands.dup
        def initialize(w_id = nil) #:nodoc: 
          @w_id = w_id
        end
        def actual_commands # :nodoc: 
          cmds = self.class.const_get(:COMMANDS).collect do |command|
            command % (@w_id.nil? ? "" : " --window #{@w_id}")
          end
          cmds
        end
      end)
    end
    
    #Special escape chars. The list is fairly uncomplete, so feel free to add chars not noted. 
    #Use the program +xev+ to determine how a sequence is escaped and map the char 
    #to the escape sequence. 
    SPECIAL_CHARS = {
      "\n" => "Return", 
      " " => "space", 
      "?" => "shift+question", 
      "!" => "shift+exclam", 
      "," => "comma", 
      "." => "period", 
      ";" => "shift+semicolon", 
      ":" => "shift+colon", 
      '"' => "shift+2", 
      "§" => "shift+3", 
      "$" => "shift+4", 
      "%" => "shift+5", 
      "&" => "shift+6", 
      "/" => "shift+7", 
      "(" => "shift+8", 
      ")" => "shift+9", 
      "=" => "shift+0", 
      "´" => "dead_acute+dead_acute", 
      "`" => "shift+dead_grave+shift+dead_grave", 
      "^" => "dead_circumflex+dead_circumflex", 
      "°" => "shift+degree", 
      "+" => "plus", 
      "*" => "shift+asterisk", 
      "#" => "numbersign", 
      "'" => "shift+apostrophe", 
      "-" => "minus", 
      "_" => "shift+underscore", 
      "<" => "less", 
      ">" => "shift+greater", 
      "\t" => "Tab", 
      "\b" => "BackSpace", 
      "ä" => "adiaeresis", 
      "Ä" => "shift+adiaeresis", 
      "ö" => "odiaeresis", 
      "Ö" => "shift+odiaeresis", 
      "ü" => "udiaeresis", 
      "Ü" => "shift+udiaeresis", 
      "ß" => "ssharp", 
      "¹" => "ISO_Level3_Shift+1", 
      "²" => "ISO_Level3_Shift+2", 
      "³" => "ISO_Level3_Shift+3", 
      "¼" => "ISO_Level3_Shift+4", 
      "½" => "ISO_Level3_Shift+5", 
      "¬" => "ISO_Level3_Shift+6", 
      "{" => "ISO_Level3_Shift+7", 
      "[" => "ISO_Level3_Shift+8", 
      "]" => "ISO_Level3_Shift+9", 
      "}" => "ISO_Level3_Shift+0", 
      "\\" => "ISO_Level3_Shift+backslash", 
      "@" => "ISO_Level3_Shift+at", 
      "€" => "ISO_Level3_Shift+EuroSign", 
      "~" => "ISO_Level3_Shift+dead_tilde+IS_Level3_Shift+dead_tilde+Left+BackSpace", #xdotool seems to have a bug here, so I need to delete the extra plus sign. 
      "|" => "ISO_Level3_Shift+bar"
    }
    
    class << self
      
      #Types a character sequence, but without any special chars. 
      #This function is a bit faster then #simulate. 
      def type(str, w_id = nil)
        out = `#{XDOTOOL} type #{w_id ? "--window #{w_id}" : ""}"#{str}"`
        nil
      end
      
      #Types a character sequence. You can use the escape sequence {...} to send special 
      #keystrokes, a full list of supported keystrokes is printed out by: 
      #  puts (XDo::Keyboard.constants - [:SPECIAL_CHARS])
      #This method recognizes many special chars like ? and ä, even if you disable 
      #the escape syntax {..} via setting the +raw+ parameter to true (that's the only way to send the { and } chars). 
      #It's a bit slower than the #type method. 
      def simulate(str, raw = false, w_id = nil)
        if raw
          commands = []
          str.each_char do |char|
            commands << "#{XDOTOOL} key #{w_id ? "--window #{w_id}" : ""}#{check_for_special_key(char)}"
          end
          
          commands.each do |cmd|
            out = `#{cmd}`
          end
          return nil
        else
          raise(XDo::XError, "Invalid number of open and close braces!") unless str.scan(/{/).size == str.scan(/}/).size
          tokens = []
          if str =~ /{/
            str.scan(/(.*?){(\w+)}/){|normal, escape| tokens << normal << escape}
            #Wenn noch Text  übrig ist, diesen noch dranhängen. 
            tokens << $' unless $'.empty?
          else
            #Keine Escape-Sequenz drin, also ist eine Spezialbehandlung überflüssig. 
            #Rekursiver Aufruf von simulate, aber als reiner Text. 
            simulate(str, true, w_id)
            return
          end
          commands = []
          tokens.each_with_index do |token, index|
            #Jeder zweite Token muss eine Escape-Sequenz sein; selbst wenn der 
            #String mit einer Escape-Sequenz anfängt, wird der Reguläre Ausdruck 
            #einen Leerstring extrahieren. 
            if index.odd? #Ein Array fängt bei Null an, das zweite Element ist Eins. 
              #Escape-Sequenz in eine Befehlsfolge umwandeln
              commands << sequence_escape(token, w_id)
              #Die eingefügten Arrays plätten, damit ein einziger Befehlsstack übrig bleibt
              commands.flatten!
            else
              #Ab hier ist der Token sicherlich normaler Text; entsprechend wird damit verfahren. 
              token.each_char do |char|
                commands << "#{XDOTOOL} key #{w_id ? "--window #{w_id}" : ""}#{check_for_special_key(char)}"
              end
              commands.flatten! #Zur Sicherheit
            end
          end
          #Ausführen aller Befehle
          commands.each do |cmd|
            out = `#{cmd}`
          end
        end
      end
      
      #Simulate a single char directly via the +key+ function of +xdotool+. 
      #+c+ is a single char like "a" or a combination like "shift+a". 
      def char(c, w_id = nil)
        `#{XDOTOOL} key #{w_id ? "--window #{w_id}" : ""}#{c}`
      end
      alias key char
      
      #Holds a key down. Please call #key_up after a call to this method. 
      def  key_down(key, w_id = nil)
        `#{XDOTOOL} keydown #{w_id ? "--window #{w_id}" : "" }#{check_for_special_key(key)}`
      end
      
      #Releases a key hold down by #key_down. 
      def key_up(key, w_id = nil)
        `#{XDOTOOL} keyup #{w_id ? "--window #{w_id}" : "" }#{check_for_special_key(key)}`
      end
      
      #Deletes a char. If +right+ is true, +del_char+ uses 
      #the DEL key for deletion, otherwise the BackSpace key. 
      def delete(right = false)
        Keyboard.simulate(right ? "\b" : "{DEL}")
      end
      
      #Allows you to things like this: 
      #  XDo::Keyboard.ctrl_c
      #The string will be capitalized and every _ will be replaced by a + and then passed into #char. 
      #You can't use this way to send whitespace or _ characters. 
      def method_missing(sym, *args, &block)
        super if args.size > 1 or block
        char(sym.to_s.capitalize.gsub("_", "+"), args[0])
      end
      
      private
      
      #Returns a key sequence in which special chars are recognized. 
      def check_for_special_key(key)
        if SPECIAL_CHARS.has_key? key
          #Spezialzeichen
          return SPECIAL_CHARS[key]
        elsif key =~/\d/
          #Ziffer
          return key
        elsif key.upcase == key
          #Großgeschrieben
          return "shift+#{key}"
        else
          #Normal
          return key
        end
      end #check_for_special_key
      
      #Wraps an escape sequenz in the corerresponding class and 
      #returns the commands needed to execute it. 
      def sequence_escape(token, w_id)
        esc = XDo::Keyboard.const_get(token.upcase.to_sym).new(w_id)
        return esc.actual_commands
      end
      
    end
    
  end
  
end