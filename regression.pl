%%%Author: Wei Edwin Chen
%  Student ID: 74545047
%  CS ID: i5g5
%  Course: CPSC 312
%  Project #1
% Element wise product of two lists.
% list_product(A,B,C) is true if each element in C is the product of corresponding elements in A and B.
list_product([],[],[]).
list_product([H1|T1],[H2|T2],[X|L3]):-list_product(T1,T2,L3), X is H1*H2.

% Sums all the elements.
% list_sum(A,B) is true if B is the sum of all elements in A.
list_sum([H|T],R):-list_sum_helper(T,H,R).
list_sum_helper([],Acc,Acc).
list_sum_helper([H|T],Acc,R):-NewAcc is H+Acc,list_sum_helper(T,NewAcc,R).

% Counts the number of elements in a list
% list_count(A,R) is true if R is the number of elements in A
list_count([_|T],R):-list_count_helper(T,1,R).
list_count_helper([],Acc,Acc).
list_count_helper([_|T],Acc,R):-NewAcc is 1+Acc,list_count_helper(T,NewAcc,R).

% Computes the average of a list.
% list_average(L,R) is true if R is the average of all elements in L
list_average(L,R):-list_sum(L,S),list_count(L,N),R is S/N.

% Subtract all list items by one number
% list_subtract(A,B,C) is true if list C is every elements in list A subtracted by a single number B
list_subtract([],_,[]).
list_subtract([H|T],E,[H1|R1]):-H1 is H-E,list_subtract(T,E,R1).

% Subtracts all the elements.
% list_diff(A,B,C) is true if list C is the element wise difference of list A and list B
list_diff([],[],[]).
list_diff([H1|T1],[H2|T2],[X|L3]):-list_diff(T1,T2,L3), X is H1-H2.

% Square all elements in the list.
% list_square(A,B) is true if all elements in B are the squared of elements in A
list_square([],[]).
list_square([H|T],[H1|R1]):-H1 is H*H,list_square(T,R1).

% Sum product of two lists.
% list_sum_product(A,B,C) is true if list C is the sum product of list A and list B
list_sum_product(L1,L2,R):-list_product(L1,L2,R1),list_sum(R1,R).

% Create a list of ones that match the size of the input list
% list_create_ones(A,B) is true if list B has the same number of ones in it as in the number of elements in A
list_create_ones([],[]).
list_create_ones([_|T],[1|R]):-list_create_ones(T,R).

% Create pairs of x values and ones
% list_create_pairs(A,B) is true if list B is a list of pairs with each pair being one element
% from list A and a one.
list_create_pairs([],[]).
list_create_pairs([H|T],[[H,1]|R]):-list_create_pairs(T,R).

% Compute X transpose times X
% compute_two_by_two(L,L1,[A,B,C,D]) is true if matrix A is [L,L1] A, B, C, D are the elements in A_Transpose * A
% A being the top left element, B being the top right element, C being the bottom left element, and D being the bettom
% right eleemnt
compute_two_by_two(L1,LOnes,[A,B,C,D]):-list_sum_product(L1,L1,A),
    list_sum_product(L1,LOnes,B),
    list_sum_product(LOnes,L1,C),
    list_sum_product(LOnes,LOnes,D).

% compute the determinant of a two by two matrix
% compute_det([A, B, C, D], Det) is true if Det is the determinant of a 2 by 2 matrix formed by [A, B, C, D] where
% A being the top left element, B being the top right element, C being the bottom left element, and D being the bettom
% right eleemnt
compute_det([A,B,C,D],Det):-Det is 1/(A*D-B*C).

% Invert the two by two matrix
% invert_two_by_two([A, B, C, D],[NA,NB],[NC,ND]) is true if [NA, NB] and [NC, ND] represent the elements in the inverted matrix of [A, B, C, D]
% where NA being the top left element, NB being the top right element, NC being the bottom left element, and ND being the bettom
% right eleemnt
invert_two_by_two([A, B, C, D],[NA,NB],[NC,ND]):-
    compute_det([A,B,C,D],Det),NA is D*Det,
    NB is -B*Det,
    NC is -C*Det,
    ND is A*Det.

% Compute two by two times the X transpose
compute_final_matrix(_,[],[],[]).
compute_final_matrix([TT1,TT2],[H|T],[R1|TR1],[R2|TR2]):-
    list_sum_product(TT1,H,R1),
    list_sum_product(TT2,H,R2),
    compute_final_matrix([TT1,TT2],T,TR1,TR2).


% Compute the slope and the intercept
compute_coefficient([V1,V2],Y,A,B):-list_sum_product(V1,Y,A),list_sum_product(V2,Y,B).

% Given a vector of independent variables and the model parameters,
% produce predictions
prediction([],_,_,[]).
prediction([H|T],A,B,[R1|R]):-R1 is A*H+B,prediction(T,A,B,R).

% Get input from user.
read_training_data(TrainingData,PredictionData):-
    write('Please input training data as a list of pairs: '),
    nl,
    read(TrainingData),
    nl,
    write("Please input predicion data as a list:"),
    nl,
    read(PredictionData).

% Seperate the X and Y pairs
pre_process_data([],[],[]).
pre_process_data([[X,Y]|T],[X|R1],[Y|R2]):-pre_process_data(T,R1,R2).

% Calculate RSS of two vectors.
rss(L1,L2,R):-
    list_diff(L1,L2,Diff),
    list_square(Diff,Squared),
    list_sum(Squared,R).

% Calculate the R squared of the model.
rs(Y,RSS,R):-
    list_average(Y,YA),
    list_subtract(Y,YA,SStotDiff),
    list_square(SStotDiff,SStotDiffSquare),
    list_sum(SStotDiffSquare,SStot),
    R is 1-(RSS/SStot).


% Fit the model and perform prediction
regression_1f():-
    read_training_data(TrainingData,PredictionData),
    pre_process_data(TrainingData,X,Y),
    list_create_ones(X,LOnes),
    list_create_pairs(X,Pairs),
    compute_two_by_two(X,LOnes,TwoByTwo),
    invert_two_by_two(TwoByTwo,FirstVector,SecondVector),
    compute_final_matrix([FirstVector,SecondVector],Pairs,FirstVectorTemp,SecondVectorTemp),
    compute_coefficient([FirstVectorTemp,SecondVectorTemp],Y,A,B),
    prediction(PredictionData,A,B,Y_bar),
    prediction(X,A,B,Y_head),
    rss(Y,Y_head,RSS),
    rs(Y,RSS,RS),
    write("Model coefficients are: "),
    write(A),
    write(" "),
    write(B),
    nl,
    write("Prediction Values are: "),
    write(Y_bar),
    nl,
    write("RSS fit quality is: "),
    write(RSS),
    nl,
    write("R square fit quality is: "),
    write(RS),
    nl.


%Testing
%[[1,1],[2,2],[3,3],[4,4],[5,5]].
%[[-4,-4],[-3,-3],[-2,-2],[-1,-1],[0,0],[1,1],[2,2],[3,3]].
%[[2,1],[4,-1],[3,-5],[1,2],[3,2],[1,1]].
