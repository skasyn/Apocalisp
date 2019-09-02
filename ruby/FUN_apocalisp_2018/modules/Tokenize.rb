Token = Struct.new(:flag, :content)

class Tokenize

  attr_accessor :tokens

  LPAR = "LPAR"
  DOT = "DOT"
  RPAR = "RPAR"
  QUOTE = "QUOTE"
  INTEGER = "INTEGER"
  SYMBOL = "SYMBOL"
  LIST = "LIST"
  BOOL = "BOOL"

  def initialize
    @tokens = []
    @error = ""
  end

  def parse_word(line, i)
    word = ""
    while !line[i].nil? && line[i] != '(' && line[i] != '.' && line[i] != ')' && line[i] != ' ' && line[i] != '\'' &&
        line[i] != '\n'
      word += line[i]
      i += 1
    end
    word.delete!("\n")
    if numeric?(word)
      @tokens << Token.new(INTEGER, word)
    else
      if word == "quote"
        @tokens << Token.new(QUOTE, "'")
      elsif word == "#t"
        @tokens << Token.new(BOOL, "true")
      elsif word == "#f"
        @tokens << Token.new(BOOL, "false")
      else
        @tokens << Token.new(SYMBOL, word) unless word == "\n"
      end
    end
    i - 1
  end

  def tokenize(line)
    i = 0
    @tokens = []
    @error = ""
    while !line[i].nil?
      if line[i] == '('
        @tokens << Token.new(LPAR, "(")
      elsif line[i] == '.'
        @tokens << Token.new(DOT, ".")
      elsif line[i] == ')'
        @tokens << Token.new(RPAR, ")")
      elsif line[i] == '\''
        @tokens << Token.new(QUOTE, "'")
      elsif line[i] != ' ' and line[i].ord != 10
        i = parse_word(line, i)
      end
      i += 1
    end
    handle_quote
    nesting(@tokens)
  end

  private
    def numeric?(char)
      res = (char =~ /[[:digit:]]/)
      !res.nil?
    end

    def letter?(char)
      res = (char =~ /[[:alpha:]]/)
      !res.nil?
    end


    def nesting(tokens)
      if tokens.size == 0
        return "ERROR"
      end
      token = tokens.delete_at(0)
      if token.flag == LPAR
        list = []
        while tokens[0].flag != RPAR
          list << nesting(tokens)
        end
        tokens.delete_at(0)
        return list
      elsif token == RPAR
        return "ERROR"
      else
        return token
      end
    end

    def handle_quote
      # This function look up for quotes token, merge them and supress the useless ones
      @tokens.each_with_index do |t, i|
        if t.flag == QUOTE
          if @tokens[i + 1].nil?
            @error = "Error : quote has no elements quoted."
          else
            to_del = []
            new_content = ""
            if @tokens[i + 1].flag == LPAR
              j = i + 1
              while @tokens[j].flag != RPAR
                if @tokens[j - 1].flag != LPAR
                  new_content << ' ' unless (j == i + 1)
                end
                new_content << @tokens[j].content
                to_del << j
                j += 1
              end
              new_content << ')'
              to_del << j
            else
              new_content = @tokens[i + 1].content
              to_del << (i + 1)
            end
            t.content = new_content
            to_del.each_with_index do |elem, i|
              @tokens.delete_at(elem)
              j = i + 1
              while !to_del[j].nil?
                to_del[j] -= 1
                j += 1
              end
            end
          end
        end
      end
      @tokens
    end
end