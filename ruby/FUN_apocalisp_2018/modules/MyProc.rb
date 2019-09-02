class MyProc
  def initialize(args, body)
    @evaluator = Evaluator.new
    @params = {}
    args.each do |arg|
      @params[arg.content] = 0
    end
    @body = body
  end

  def call(new_params)
    i = 0
    @params.each do |key, value|
      @evaluator.symbols[key] = new_params[i]
      i += 1
    end
    @evaluator.evaluate(@body)
  end
end