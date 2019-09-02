require_relative 'MyProc'

List = Struct.new(:lvalue, :rvalue) do
  def is_empty?
    if :lvalue.nil? && :rvalue.nil?
      return true
    end
    false
  end
end

class Evaluator

  attr_accessor :symbols

  def initialize
    @symbols = {}
    @symbols["cons"] = cons
    @symbols["quote"] = quote
    @symbols["car"] = car
    @symbols["cdr"] = cdr
    @symbols["+"] = add
    @symbols["-"] = sub
    @symbols["*"] = mul
    @symbols["div"] = div
    @symbols["mod"] = mod
    @symbols["<"] = inferior
    @symbols["list"] = list
    @symbols["<="] = infequal
    @symbols[">="] = supequal
    @symbols[">"] = superior
    @symbols["abs"] = absolute
    @symbols["or"] = or_binary
    @symbols["and"] = and_binary
  end

  def evaluate(tokens)
    if tokens.class == Token
      if tokens.flag == Tokenize::SYMBOL
        if @symbols[tokens.content].nil?
          puts "Exception: variable #{tokens.content} is not bound."
          return "ERROR"
        else
            return @symbols[tokens.content]
        end
      elsif tokens.flag == Tokenize::INTEGER || tokens.flag == Tokenize::QUOTE
        return tokens.content
      else
        puts "Exception: Attempt to apply non-procedure #{tokens.content}."
        return "ERROR"
      end
    end
    if tokens[0].class == Token
      if tokens[0].content == "lambda"
        return MyProc.new(tokens[1], tokens[2])
      elsif tokens[0].content == "define"
        @symbols[tokens[1].content] = evaluate(tokens[2])
        return @symbols[tokens[1].content]
      elsif tokens[0].flag == Tokenize::QUOTE
        return tokens[0].content
      end
      if @symbols[tokens[0].content].nil?
        puts "Exception: variable #{tokens[0].content} is not bound."
        return "ERROR"
      end
    end
    if tokens[0].class == Array
      tokens[0] = evaluate(tokens[0])
    end
    args = []
      for elem in tokens do
        args << evaluate(elem) unless elem == tokens[0]
      end
    if tokens[0].class == MyProc
      tokens[0].call(args)
    else
      res = @symbols[tokens[0].content].call(args)
    end
  end

  private

    def cons
      lambda do |arg|
        list = List.new(arg[0], arg[1])
      end
    end

    def quote
      lambda do |arg|
        arg[0]
      end
    end

    def car
      lambda do |arg|
        if arg[0].class != List
          puts "Error : car param is not a list."
          return "#f"
        end
        arg[0].lvalue
      end
    end

    def cdr
      lambda do |arg|
        if arg[0].class != List
          puts "Error : cdr param is not a list."
          return "#f"
        end
        arg[0].rvalue
      end
    end

    def list
      lambda do |arg|
        for elem in arg
          puts elem
        end
      end
    end

    # Arithmetics builtins
    def add
      lambda do |arg|
        if arg[0].class != String
          puts "Exception in +: #{arg[0]} is not a number"
          return "ERROR"
        elsif arg[1].class != String
          puts "Exception in +: #{arg[1]} is not a number"
          return "ERROR"
        end
        (arg[0].to_i + arg[1].to_i).to_s
      end
    end

    def sub
      lambda do |arg|
        if arg[0].class != String
          puts "Exception in -: #{arg[0]} is not a number"
          return "ERROR"
        elsif arg[1].class != String
          puts "Exception in -: #{arg[1]} is not a number"
          return "ERROR"
        end
        (arg[0].to_i - arg[1].to_i).to_s
      end
    end

    def mul
      lambda do |arg|
        if arg[0].class != String
          puts "Exception in *: #{arg[0]} is not a number"
          return "ERROR"
        elsif arg[1].class != String
          puts "Exception in *: #{arg[1]} is not a number"
          return "ERROR"
        end
        (arg[0].to_i * arg[1].to_i).to_s
      end
    end

    def div
      lambda do |arg|
        if arg[0].class != String
          puts "Exception in div: #{arg[0]} is not a number"
          return "ERROR"
        elsif arg[1].class != String
          puts "Exception in div: #{arg[1]} is not a number"
          return "ERROR"
        elsif arg[1].to_i == 0
          puts "Exception in div: undefined for 0"
          return "ERROR"
        end
        (arg[0].to_i / arg[1].to_i).to_s
      end
    end

    def mod
      lambda do |arg|
        if arg[0].class != String
          puts "Exception in mod: #{arg[0]} is not a number"
          return "ERROR"
        elsif arg[1].class != String
          puts "Exception in mod: #{arg[1]} is not a number"
          return "ERROR"
        elsif arg[1].to_i == 0
          puts "Exception in mod: undefined for 0"
          return "ERROR"
        end
        (arg[0].to_i % arg[1].to_i).to_s
      end
    end

    def inferior
      lambda do |arg|
        if arg[0].class != String
          puts "Exception in <: #{arg[0]} is not a real number"
          return "ERROR"
        elsif arg[1].class != String
          puts "Exception in <: #{arg[1]} is not a real number"
          return "ERROR"
        end
        if arg[0].to_i < arg[1].to_i
          return "#t"
        else
          return "#f"
        end
      end
    end

    def infequal
      lambda do |arg|
        if arg[0].class != String
          puts "Exception in <=: #{arg[0]} is not a real number"
          return "ERROR"
        elsif arg[1].class != String
          puts "Exception in <=: #{arg[1]} is not a real number"
          return "ERROR"
        end
        if arg[0].to_i <= arg[1].to_i
          return "#t"
        else
          return "#f"
        end
      end
    end

    def supequal
      lambda do |arg|
        if arg[0].class != String
          puts "Exception in >=: #{arg[0]} is not a real number"
          return "ERROR"
        elsif arg[1].class != String
          puts "Exception in >=: #{arg[1]} is not a real number"
          return "ERROR"
        end
        if arg[0].to_i >= arg[1].to_i
          return "#t"
        else
          return "#f"
        end
      end
    end

    def superior
      lambda do |arg|
        if arg[0].class != String
          puts "Exception in >: #{arg[0]} is not a real number"
          return "ERROR"
        elsif arg[1].class != String
          puts "Exception in >: #{arg[1]} is not a real number"
          return "ERROR"
        end
        if arg[0].to_i < arg[1].to_i
          return "#t"
        else
          return "#f"
        end
      end
    end

    def absolute
      lambda do |arg|
        if arg[0].class != String
          puts "Exception in abs: #{arg[0]} is not a real number"
          return "ERROR"
        end
        if arg[0].to_i < 0
          return ("-" + arg[0])
        else
          return arg[0]
        end
      end
    end

    def or_binary
      lambda do |arg|
        if arg[0].present? or arg[0] == "#f"
          return arg[1]
        end
        return arg[0]
      end
    end

    def and_binary
      lambda do |arg|
        if arg[0].present? or arg[0] == "#f"
          return arg[0]
        end
        return arg[1]
      end
    end

    def not_binary
      lambda do |arg|
        if arg[0].class != String
          puts "Exception in and: #{arg[0]} is not a valid value"
          return "ERROR"
        end
        if arg[0].present? or arg[0] != "#f"
          return "#t"
        end
        return "#f"
      end
    end

    def equals
      lambda do |arg|
        if arg.size != 2
          puts "Exception: incorrect argument count in call for eq?"
          return "ERROR"
        end
        if arg[0] == arg[2]
        end
      end
    end
end