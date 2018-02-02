:- use_module(library(pce)).

%Fact_for_materials
base_liquor(vodka).
base_liquor(gin).
base_liquor(rum).
base_liquor(tequila).
base_liquor(whiskey).
base_liquor(brandy).

juice(coconat_juice).
juice(orange_juice).
juice(lemonade).
juice(lime_juice).
juice(tomato_juice).

condiment(milk).
condiment(soda_water).
condiment(water).
condiment(cola).
condiment(coffee).
condiment(mint_leaf).
condiment(X) :- juice(X).

liqueur(kahlua).
liqueur(baileys_cream).
liqueur(creme_de_menthe).
liqueur(amarula).
liqueur(vermouth).

syrup(grenadine).
syrup(basic_syrup).
syrup(ginger).

dislike(ginger).

ingredient(X) :- base_liquor(X).
ingredient(X) :- condiment(X).
ingredient(X) :- liqueur(X).
ingredient(X) :- syrup(X).

glass(bullet_cup).
glass(martini_cup).
glass(collins_glass).
glass(old_fashion_glass).
glass(brandy_snifter).

color(white).
color(yellow).
color(black).
color(red).
color(brown).
color(transparent).

category(sours).
category(collins).
category(old_fashion).
category(three_partners).

bitter(vermouth).
bitter(coffee).
bitter(kahlua).
sweet(creme_de_menthe).
sweet(amarula).
sweet(baileys_cream).
sweet(cola).
bitter(X) :- syrup(X).
sweet(X) :- juice(X).

result(tequila_sunrise).
result(b52).
result(white_russian).
result(black_russian).
result(long_island_iced_tea).
result(mojito).
result(singapore_sling).
result(bloody_mary).
result(whiskey_rickey).
result(whiskey_sling).
result(martini).
result(vodka_espresso).
result(tequila_shot).
%result(system_create).

degree(high).
degree(low).
degree(medium).

display(cannot_burn).
display(can_burn).
display(cannot_divide).
display(can_divide).

cocktail_pic(tequila_sunrise, './image/tequila_sunrise.jpg').
cocktail_pic(b52, './image/b52.jpg').
cocktail_pic(white_russian, './image/white_russian.jpg').
cocktail_pic(black_russian, './image/black_russian.jpg').
cocktail_pic(long_island_iced_tea, './image/long_island.jpg').
cocktail_pic(mojito, './image/mojito.jpg').
cocktail_pic(singapore_sling,'./image/singapore.jpg').
cocktail_pic(bloody_mary, './image/mary.jpg').
cocktail_pic(whiskey_rickey, './image/whiskey_ricker.jpg').
cocktail_pic(whiskey_sling,'./image/whiskey_sling.jpg').
cocktail_pic(martini, './image/martini.jpg').
cocktail_pic(vodka_espresso, './image/vodka_espresso.jpg').
cocktail_pic(tequila_shot,'./image/tequila_shot.jpg').
cocktail_pic(system_create, './image/system_create.jpg').

input1('high', Materials) :- high_degree(Materials).
input1('low', Materials) :- low_degree(Materials); normal_degree(Materials).
input1('whatever', Materials) :- contain_alcohol(Materials).
input2('whatever',Materials) :- contain_alcohol(Materials).
input2(X, Y) :- category(X), divide_category(X, Y).
input3('yes', Materials) :- burn(Materials).
input3('no', Materials) :- \+burn(Materials).
input3('whatever', Materials) :- contain_alcohol(Materials).
input4('yes', Materials) :- divide(Materials).
input4('no', Materials) :- \+divide(Materials).
input4('whatever', Materials) :- contain_alcohol(Materials).
input5('whatever', Materials) :- contain_alcohol(Materials).
input5(X, Materials) :- glass(X), cup(X, Materials).

find(Cocktail, Materials) :- cocktail(Cocktail, Materials).

%library
member(X, [X|_]).
member(X, [_|T]) :- member(X,T).
len([], 0).
len([_|T], N) :- len(T,X), N is X+1.

%basic_rule
contain_alcohol(T) :- member(X,T), base_liquor(X); member(Y, T), liqueur(Y).
contain_condiment(T) :- member(X, T), condiment(X),!.
base_liquor_num([], 0).
base_liquor_num([X|Z], Y) :- base_liquor_num(Z, N), base_liquor(X), Y is N+1.
base_liquor_num([X|Z], Y) :- base_liquor_num(Z, N), \+base_liquor(X), Y is N.
condiment_num([], 0).
condiment_num([X|Z], Y) :- condiment_num(Z, N), condiment(X), Y is N+1.
condiment_num([X|Z], Y) :- condiment_num(Z, N), \+condiment(X), Y is N.
legal([]).
legal([X|T]) :- ingredient(X), legal(T).

%formula
cocktail(tequila_sunrise, X) :- X = [grenadine, tequila, orange_juice].
cocktail(b52, X) :- X = [kahlua, vodka, baileys_cream].
cocktail(white_russian, X) :- X = [vodka, kahlua, milk].
cocktail(black_russian, X) :- X = [vodka, kahlua].
cocktail(long_island_iced_tea, X) :- X = [gin, vodka, tequila, rum, lemonade, cola, basic_syrup, creme_de_menthe].
cocktail(mojito, X) :- X = [rum, soda_water, basic_syrup, mint_leaf, lime_juice].
cocktail(singapore_sling, X) :- X = [gin, brandy, lemonade, soda_water, base_liquor].
cocktail(bloody_mary, X) :- X = [vodka, tomato_juice].
cocktail(whiskey_rickey, X) :- X = [whiskey, soda_water, lime_juice].
cocktail(whiskey_sling, X) :- X = [lemonade, whiskey, water].
cocktail(martini, X) :- X = [gin].
cocktail(vodka_espresso, X) :- X = [vodka, coffee, amarula].
cocktail(tequila_shot, X) :- X = [tequila].
cocktail(system_create, T) :- contain_alcohol(T), \+cocktail(tequila_sunrise, T), \+cocktail(b52, T),\+cocktail(white_russian, T),
                              \+cocktail(black_russian, T), \+cocktail(long_island_iced_tea, T),
							  \+cocktail(mojito, T), \+cocktail(singapore_sling,T),
							  \+cocktail(bloody_mary, T), \+cocktail(whiskey_rickey, T),
							  \+cocktail(whiskey_sling, T),\+cocktail(martini, T),
							  \+cocktail(vodka_espresso, T),\+cocktail(tequila_shot, T),len(T, X), X <6, legal(T).

%degree
high_degree(X) :- shot(X).
high_degree(X) :- base_liquor_num(X, Y), Y > 2.
high_degree(X) :- len(X, 2), member(Z, X), base_liquor(Z).
low_degree(X) :- \+high_degree(X), condiment_num(X, Y), Y > 2.
normal_degree(X) :- \+high_degree(X), \+low_degree(X).
get_degree(X, high) :- high_degree(X).
get_degree(X, low) :- low_degree(X).
get_degree(X, medium) :- normal_degree(X).

%drink
shot(X) :- cocktail(b52, X).
shot(T) :- len(T, 1), base_liquor_num(T, 1).
long_drink(X) :- low_degree(X); normal_degree(X).
short_drink(X) :- high_degree(X), \+shot(X).

%categories
divide_category(sours, X) :- member(Z, X), syrup(Z), member(Y, X), base_liquor(Y), member(A, X), juice(A).
divide_category(collins, X) :- member(Z, X), syrup(Z), member(lime_juice, X);member(Z, X), syrup(Z), member(lemonade, X).
divide_category(old_fashion, X) :- member(Z, X), bitter(Z), member(Y, X), sweet(Y).
divide_category(three_partners, X) :- len(X, 3), member(Z, X), base_liquor(Z), member(A, X), liqueur(A).

%color
match_color(X, white) :- X = milk; X = coconat_juice; X = baileys_cream.
match_color(X, yellow) :- X = lemonade; X = orange_juice; X = ginger.
match_color(X, black) :- X = cola; X = coffee; X = kahlua.
match_color(X, red) :- X = tomato_juice; X = grenadine.
match_color(X, brown) :- X = amarula.
match_color(X, transparent) :- \+match_color(X, white), \+match_color(X, yellow),
                   \+match_color(X, black), \+match_color(X, red), \+match_color(X, brown).
color_equal(X, Y) :- color(Z), match_color(X, Z), match_color(Y, Z).

%density
density_stronger_basic(kahlua, baileys_cream).
density_stronger_basic(X, Y) :- syrup(X), liqueur(Y).
density_stronger_basic(X, Y) :- liqueur(X), condiment(Y).
density_stronger_basic(X, Y) :- liqueur(X), base_liquor(Y).
density_stronger(X, Z) :- density_stronger_basic(X,Z); ingredient(Y), density_stronger_basic(X, Y), density_stronger(Y, Z).
divide(X) :- ingredient(Y), member(Y, X), ingredient(Z), member(Z, X), density_stronger(Y, Z), not(color_equal(Y, Z)),!.
divide_output(X, can_divide) :- divide(X).
divide_output(X, cannot_divide) :- \+divide(X).

%burn
burn(X) :- len(X, 1), base_liquor(Z), member(Z, X).
burn(X) :- divide(X), base_liquor(Y), member(Y, X), not(contain_condiment(X)), base_liquor_num(X, 1).
burn_output(X, can_burn) :- burn(X).
burn_output(X, cannot_burn) :- \+burn(X).

%glass
cup(bullet_cup, X) :- \+cocktail(martini, X), shot(X); \+cocktail(martini, X), len(X, 1), X = [N|_], base_liquor(N).
cup(martini_cup, X) :- len(X, Y), Y < 5, cocktail(martini, X); len(X, Y), Y < 5, short_drink(X).
cup(brandy_snifter, X) :- base_liquor_num(X, 1), member(brandy, X).
cup(collins_glass, X) :- long_drink(X), \+cup(brandy_snifter, X), \+cocktail(martini, X).
cup(old_fashion_glass, X) :- short_drink(X),\+shot(X).

%entry
intro:- new(Dialog, dialog('Cocktail Expert System')),
		send(Dialog, append, new(Digree, text_item(digree, 'whatever'))),
        new(ValueSet1, chain('high', 'low', 'whatever')),
        send(Digree, value_set, ValueSet1),

        send(Dialog, append, new(Category, text_item(category, 'whatever'))),
        new(ValueSet2, chain('sours','collins','old_fashion','three_partners','whatever')),
        send(Category, value_set, ValueSet2),

        send(Dialog, append, new(Burn, text_item(burn, 'whatever'))),
        new(ValueSet3, chain('yes','no','whatever')),
        send(Burn, value_set, ValueSet3),

        send(Dialog, append, new(Divide, text_item(divide, 'whatever'))),
        new(ValueSet4, chain('yes','no','whatever')),
        send(Divide, value_set, ValueSet4),

        send(Dialog, append, new(Glass, text_item(glass, 'whatever'))),
        new(ValueSet5, chain('bullet_cup','martini_cup','collins_glass','old_fashion_glass','brandy_snifter','whatever')),
        send(Glass, value_set, ValueSet5),

        send(Dialog, append, button('GET COCKTAIL',
		      message(@prolog,test,Digree?selection, Category?selection, Burn?selection, Divide?selection, Glass?selection))),
        send(Dialog, append, button('SPECIAL FOR YOU',
		      message(@prolog, generate, Digree?selection, Category?selection, Burn?selection, Divide?selection, Glass?selection))),
		send(Dialog, append, button('Close',
		      message(@prolog,cancel,Dialog))),
		send(Dialog, open).

test(Digree, Category, BurnOrNot, DivideNot, CupKind):-
		result(Cocktail), find(Cocktail, Materials),
		input1(Digree, Materials),
		input2(Category, Materials),
        input3(BurnOrNot, Materials),
		input4(DivideNot, Materials),
        input5(CupKind, Materials),
		\+dislike(Cocktail),
		describe(Cocktail, Materials),!.

generate(Digree, Category, BurnOrNot, DivideNot, CupKind) :-
        find(system_create, Materials),
		input1(Digree, Materials),
		input2(Category, Materials),
        input3(BurnOrNot, Materials),
		input4(DivideNot, Materials),
        input5(CupKind, Materials),
		describe(system_create, Materials),!.

cancel(Dialog) :- send(Dialog, destroy).

describe(Cocktail, Materials) :-
                   new(Dialog, dialog), new(Picture, picture),
				   new(Window, frame('Cocktail Card')),
				   send(Window, append, Dialog),
				   send(Dialog, below, Picture),
				   make_picture(Picture, Cocktail, Materials),
				   make_dialog(Dialog, Window, Cocktail),
				   send(Window, open).

make_dialog(Dialog, Window, system_create) :-
				   send(Dialog, append, button('Close', message(@prolog,cancel, Window))).

make_dialog(Dialog, Window, Cocktail) :- Cocktail \= system_create,
				   send(Dialog, append, button('Dislike', message(@prolog, add, Cocktail))),
				   send(Dialog, append, button('Close', message(@prolog,cancel, Window))).

make_picture(Result_page, Cocktail, Materials) :- glass(X), cup(X, Materials),
				   degree(Y), get_degree(Materials, Y),
				   display(Text1), divide_output(Materials, Text1),
				   display(Text2), burn_output(Materials, Text2),
                   send(Result_page, display, text('Cocktail Name:'), point(20, 20)),
				   send(Result_page, display, text(Cocktail), point(120, 20)),
				   send(Result_page, display, text('         Material:'), point(20, 40)),
				   show(Result_page, Materials, 120),
				   send(Result_page, display, text('         Digree:'), point(20, 60)),
				   send(Result_page, display, text(Y), point(120, 60)),
				   send(Result_page, display, text('      Use glass:'), point(20, 80)),
				   send(Result_page, display, text(X), point(120, 80)),
				   send(Result_page, display, text('      Attribute:'), point(20, 100)),
				   send(Result_page, display, text(Text1), point(120, 100)),
				   send(Result_page, display, text(Text2), point(120, 120)),
				   cocktail_pic(Cocktail, Pic),
				   new(Image, bitmap(Pic)),
                   send(Result_page, display, Image,point(40,140)).

show(Page, [X|T], Position) :- send(Page, display, text(X), point(Position, 40)),
                               Last_position is Position + 80, show(Page, T, Last_position).
show(Page, [], Position) :- send(Page, display, text(' '), point(Position, 40)).

add(Cocktail) :- assertz(dislike(Cocktail)).
