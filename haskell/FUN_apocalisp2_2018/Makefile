##
## EPITECH PROJECT, 2019
## apocalisp 2
## File description:
## makefile of project
##

CC	=	ghc

SRC	=	main.hs		\
		Apocalisp.hs	\
		ApocaFiles.hs

NAME	=	alisp2

all	:	$(NAME)

$(NAME)	:
	$(CC) -o $(NAME) $(SRC)

clean	:
	$(RM) $(SRC:.hs=.o)

fclean	:	clean
	$(RM) $(NAME)

re	: fclean all

.PHONY	: all clean fclean re
