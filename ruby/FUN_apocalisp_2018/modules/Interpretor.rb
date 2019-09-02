require_relative 'Tokenize'
require_relative 'Eval_expr'

class Interpretor

  attr_accessor :tokens, :error

  def initialize
    @tokenizer = Tokenize.new
    @evaluator = Evaluator.new
    @errors = ""
    @idx = 0
  end

  def lisp_interpretor
    #puts "Welcome to Wallisp, the wallit-made Lisp interpretor !\n" +
    #         "Press Ctrl + D or type exit to quit the program.\n"
    #print '> '
    while !(line = gets).nil? && line != "exit\n" && line != "\n"
      parse_it(line)
     # print '> '
    end
    #puts "Quitting interpretor ..."
  end

  private

    def format_nested(res)
      if res.class != List
        print res
      else
        print '('
        if res.lvalue.class == List
          format_nested(res.lvalue)
        else
          print res.lvalue
        end
        print ' . '
        if res.rvalue.class == List
          format_nested(res.rvalue)
        else
          print res.rvalue
        end
        print ')'
      end
    end

    def format_res(res)
      if res == "ERROR"
        return
      end
      format_nested(res)
      puts
    end

    def handle_token
      if tokens.flag == Tokenize::LPAR
      end
    end

    def parse_it(line)
      @idx = 0
      tokens = @tokenizer.tokenize(line)
      res = @evaluator.evaluate(tokens)
      format_res(res)
    end
end