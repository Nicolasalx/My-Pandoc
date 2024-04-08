##
## EPITECH PROJECT, 2024
## B-FUN-400-PAR-4-1-mypandoc-thibaud.cathala
## File description:
## Makefile
##

NAME		=	mypandoc

BIN_PATH	:=	$(shell stack path --local-install-root --resolver=lts-20.11)

NAME_TEST 	= 	unit_tests

$(NAME):
	stack build --resolver=lts-20.11
	cp $(BIN_PATH)/bin/$(NAME)-exe $(NAME)

all: $(NAME)

clean:
	stack clean --resolver=lts-20.11

fclean: clean
	rm -f $(NAME)

re: fclean all

unit_tests:

tests_run:
	stack test --resolver=lts-20.11

.PHONY: all clean fclean re unit_tests tests_run
