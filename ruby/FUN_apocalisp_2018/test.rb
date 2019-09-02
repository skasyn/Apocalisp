module GetGlob
  def retrieve_glob
    srcs = []
    Dir.glob("./**/*.c").each {|path| srcs << path}
    srcs
  end
end