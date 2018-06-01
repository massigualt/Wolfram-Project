(* ::Package:: *)

(* :Title: Progetto del corso di Matematica Computazionale/Calcolo Numerico e Software Didattico, AA 2017/2018, Universita' degli Studi di Bologna *)
(* :Software: Wolfram Mathematica 11.2.0 *)
(* :Context: `Progetto` *)
(* :Author:  Massimiliano Gualtieri, Paolo Santilli, Andrea Morabito, Chiara Ienna *)
(* :Summary:  *)



BeginPackage["Progetto`"];  (* Definizione e inizio del package *)

Unprotect["Progetto`*"];(* Per permettere il ricaricamento del package, rimuoviamo l'attributo Protected dai simboli del Package *)

ClearAll["Progetto`*"]; (* e cancelliamo le vecchie definizioni *)



(* Tutte le funzionalita' esportate pubblicamente sono elencate di seguito *)
slideView1::usage = "Inserisce una slideview per mostrare esempi di disequazioni";
slideView2::usage = "Inserisce una slideview per la rappresentazione di disequazioni in forma normale";

allenamento::usage = "Plot con studio dei segni della disequazione";

getListaDisequazioni::usage = "Ritorna la lista delle disequazioni";
getListaEquazioni::usage = "Ritorna la lista delle equazioni";

manipulateDis::usage = "Manipulate per la rappresentazione della disequazione di primo grado";
manipulateDis2::usage = "Manipulate per la rappresentazione della disequazione di secondo grado";
manipulateDis3::usage = "Manipulate per la rappresentazione della disequazione di terzo grado o superiore";
manipulateDisFratta::usage = "Manipulate per la rappresentazione della disequazione fratta";
manipulateDisIrraz::usage = "Manipulate per la rappresentazione della disequazione irrazionale";

risolviInputEq::usage = "Disegna il plot dell'equazione presa in input, una volta cliccato sui bottoni fa la numberline plot della disequazione e ne mostra le soluzioni";

gioco1::usage = "Input Studio segni, determinare la disequazione esatta";
gioco2::usage = "Input disequazione, trovare giusto studio dei segni";
gioco3::usage = "Input grafico, trovare il giusto studio dei segni per x>0 ";
gioco4::usage = "Input grafico, trovare il giusto studio dei segni x>=0";
gioco5::usage = "Input grafico, trovare il giusto studio dei segni x<0";
gioco6::usage = "Input grafico, trovare il giusto studio dei segni x<=0";
scegliGioco::usage = "Ritorna un esercizio random dalla lista dei giochi possibili";

checkRisp::usage = "Controlla se la risposta \[EGrave] corretta o no";
newExercise::usage = "Bottone per generare un nuovo esercizio Random";

(* serve ad evitare problemi nella print delle equazioni *)
x::usage = "";


Begin["`Private`"];  (* Inizio area privata con implentazione delle funzionalita'*)

SetDirectory[NotebookDirectory[]]; (* imposto la cartella attuale come base in cui cercare i file *)


(*
  slideview per mostrare esempi di disequazioni
*)
slideView1[] := (*Inserisco le disequazioni da mostrare*)
    SlideView[{x + 1 >= 2 \[CenterDot] x, x^2 - 2 \[CenterDot] x <= x + 4,
      3 \[CenterDot] x^2 + x < x^5 - 2 \[CenterDot] x^3,
      (x^2 - 1)^3 / (x + 10) >= 2 \[CenterDot] x - 4,
      Sqrt[x^2 + 2 \[CenterDot] x - 3] > 3 \[CenterDot] x + 1},
      AppearanceElements -> {"FirstSlide", "PreviousSlide", "NextSlide", (*Imposto le proprieta' dello slideview*)
        "LastSlide", "SliderControl", "SlideNumber", "SlideTotal"},
      ImageSize -> Scaled[.5], Alignment -> Center]

(*
  slideview per mostrare disequazioni in forma normale
*)
slideView2[]:= (*Inserisco le disequazioni da mostrare*)
    SlideView[ {x + 1 >= 2 \[CenterDot] x  \[DoubleRightArrow]  -x + 1 >= 0,
      x^2 - 2 \[CenterDot] x <= x + 4  \[DoubleRightArrow]
      x^2 - 3 \[CenterDot] x - 4 <= 0,
      3 \[CenterDot] x^2 + x <
          x^5 - 2 \[CenterDot] x^3  \[DoubleRightArrow]
      x^5 - 2 \[CenterDot] x^3 - 3 \[CenterDot] x^2 - x > 0,
      (x^2 - 1)^3/(x + 10) >=
          2 \[CenterDot] x -
              4   \[DoubleRightArrow]   (x^2 - 1)^3/(x + 10) -
              2 \[CenterDot] x + 4 >= 0,
      Sqrt[x^2 + 2 \[CenterDot] x - 3] >
          3 \[CenterDot] x + 1  \[DoubleRightArrow]
      Sqrt[x^2 + 2 \[CenterDot] x - 3] - 3 \[CenterDot] x - 1 > 0} ,
      AppearanceElements -> {"FirstSlide", "PreviousSlide", "NextSlide", (*Imposto le proprieta' dello slideview*)
        "LastSlide", "SliderControl", "SlideNumber", "SlideTotal"} ,
      ImageSize -> Scaled[.6], Alignment -> Center]


allenamento[func_] :=

(*
	Riga con il plot dell'equazione e lo studio dei segni della disequazione
*)
    Row[{ pointColor = LightGray; labelColor = White;

    (* Stampo un messaggio per il corretto funzionamento *)
    title = Text[Style["\t\t\t\t\tClicca su un bottone per aggiornare", Red, Italic, 24]];
    Print[title];

    Dynamic[
    (* Colonna contenente il plot della funzione inserita *)
      Column[{
        fun1 = Plot[func, {x, -10, 10},
          PlotStyle -> {Automatic, Red},
          PlotLabel -> Style[func, labelColor],
          Filling -> Axis,
          FillingStyle -> {nosol, sol},
          Mesh -> {{0}}, (*Controlliamo i punti di intersezione con l'asse x*)
          MeshFunctions -> {#2&},
          MeshStyle -> Directive[pointColor, PointSize[.02]],
          ImageSize -> 500];(*Propriet\[AGrave] per settare la grandezza della colonna*)
        Show[fun1]
      }]


    ], "     "
    (* Colonna contenente i vari bottoni con <, \[LessEqual], >, \[GreaterEqual], all'occorrenza cambiamo il colore dei punti e della label*)
      Column[{

        Column[{
          Button["\!\(\*
StyleBox[\">\",\nFontWeight->\"Bold\"]\)", dis = func > 0; sol = LightBlue; nosol = White; pointColor = LightGray; labelColor = Gray, ImageSize -> 30],
          Button["\!\(\*
StyleBox[\"\[GreaterEqual]\",\nFontWeight->\"Bold\"]\)", dis = func >= 0; sol = LightBlue; nosol = White; pointColor = Green; labelColor = Gray, ImageSize -> 30],
          Button["\!\(\*
StyleBox[\"<\",\nFontWeight->\"Bold\"]\)", dis = func < 0; sol = White; nosol = LightBlue; pointColor = LightGray; labelColor = Gray, ImageSize -> 30],
          Button["\!\(\*
StyleBox[\"\[LessEqual]\",\nFontWeight->\"Bold\"]\)", dis = func <= 0; sol = White; nosol = LightBlue; pointColor = Green; labelColor = Gray, ImageSize -> 30]

        }]

      (* Colonna contenente le soluzioni della funzione inserita *)
        Column[{
          Dynamic[
            NumberLinePlot[dis, {x, -10, 10},
              PlotStyle -> {Automatic, Red},
              PlotLabel -> Style[
                ToString[TraditionalForm[dis]]"\n \!\(\*StyleBox[\"Soluzioni\",\nFontWeight->\"Bold\"]\): "
                    <> ToString[TraditionalForm[Reduce[dis, x]]] "\n" , labelColor],
              ImageSize -> 500]

          ]

        }]
      }]
    }];

(* 
	Disegna il plot dell'equazione presa in input,
	una volta cliccato sui bottoni fa la numberline plot della disequazione e
	ne mostra le soluzioni
*)

risolviInputEq[] := Panel[
(*imposto i colori della label e del punto*)
  labelColorRisolviInputEq = White;
  pointColorRisolviInputEq = LightGray;
  DynamicModule[{f = "inserisci un'espressione"},
    Column[{InputField[Dynamic[f]],
      Button["\!\(\*
StyleBox[\">\",\nFontWeight->\"Bold\"]\)", disInput = f > 0; labelColorRisolviInputEq = Gray; sol2 = LightBlue; nosol2 = White; pointColorRisolviInputEq = LightGray],
      Button["\!\(\*
StyleBox[\"\[GreaterEqual]\",\nFontWeight->\"Bold\"]\)", disInput = f >= 0; labelColorRisolviInputEq = Gray; sol2 = LightBlue; nosol2 = White; pointColorRisolviInputEq = Green],
      Button["\!\(\*
StyleBox[\"<\",\nFontWeight->\"Bold\"]\)", disInput = f < 0; labelColorRisolviInputEq = Gray; sol2 = White; nosol2 = LightBlue; pointColorRisolviInputEq = LightGray],
      Button["\!\(\*
StyleBox[\"\[LessEqual]\",\nFontWeight->\"Bold\"]\)", disInput = f <= 0; labelColorRisolviInputEq = Gray; ; sol2 = White; nosol2 = LightBlue; pointColorRisolviInputEq = Green]
    }]
  (*plot dell'equazione e visualizzazione dei punti di intersezione con l'asse x*)
    Column[{
      Dynamic[ Plot[f, {x, -20, 20} ,
        PlotStyle -> {Automatic, Red},
        PlotLabel -> Style[f, Gray],
        Filling -> Axis,
        FillingStyle -> {nosol2, sol2},
        Mesh -> {{0}},
        MeshFunctions -> {#2&},
        MeshStyle -> Directive[pointColorRisolviInputEq, PointSize[.02]],
        ImageSize -> 400]]
    }]
  (*visualizzazione della numberLinePlot*)
    Column[{
      Dynamic[Row[{"    "}]],
      Dynamic[ NumberLinePlot[disInput, {x, -20, 20},
        PlotLabel -> Style[ToString[TraditionalForm[disInput]]"\n \!\(\*StyleBox[\"Soluzioni\",\nFontWeight->\"Bold\"]\): "
            <> ToString[TraditionalForm[Reduce[disInput, x]]] "\n" , labelColorRisolviInputEq],
        ImageSize -> 400]]
    }]
  ]
];




(*
	Manipulate per visualizzare una NumberLinePlot di una disequazione di primo grado
*)
manipulateDis[] := Manipulate[
  NumberLinePlot[f[a x + b , 0] , {x, -10, 10},
    PlotLabel -> Style[f[a x + b , 0]  , 16, Red]],
  {{a, 2, "a"}, -10, 10}, (*Valori della manipulate*)
  {{b, 2, "b"}, -10, 10},
  {{f, Greater, "f(x)"}, {Greater, GreaterEqual, Less, LessEqual},
    ControlType -> PopupMenu }
];

(*
	Manipulate per visualizzare una NumberLinePlot di una disequazione di secondo grado
*)
manipulateDis2[] := Manipulate[
  NumberLinePlot[f[a x^2 + b x + c, 0] , {x, -10, 10},
    PlotLabel -> Style[f[a x^2 + b x + c, 0]  , 16, Red]],
  {{a, 2, "a"}, -10, 10}, (*Valori della manipulate*)
  {{b, 2, "b"}, -10, 10},
  {{c, -2, "c"}, -10, 10},
  {{f, Greater, "f(x)"}, {Greater, GreaterEqual, Less, LessEqual},
    ControlType -> PopupMenu }
];

(*
	Manipulate per visualizzare una NumberLinePlot di una disequazione di secondo grado
*)
manipulateDis3[] := Manipulate[
  NumberLinePlot[f[a x^5 + b x^4 + c x^3 + d x^2 + e x + g, 0] , {x, -10, 10},
    PlotLabel -> Style[f[a x^5 + b x^4 + c x^3 + d x^2 + e x + g, 0]  , 16, Red]],
  {{a, 0, "a"}, -10, 10}, (*Valori della manipulate*)
  {{b, 0, "b"}, -10, 10},
  {{c, 2, "c"}, -10, 10},
  {{d, -3, "d"}, -10, 10},
  {{e, + 5, "e"}, -10, 10},
  {{g, -2, "g"}, -10, 10},
  {{f, Greater, "f(x)"}, {Greater, GreaterEqual, Less, LessEqual}, ControlType -> PopupMenu }
];

(*
	Manipulate per visualizzare una NumberLinePlot di una disequazione fratta
*)
manipulateDisFratta[] := Manipulate[
  NumberLinePlot[f[(a x^2 + b x + c) / (d x + e), 0] , {x, -10, 10},
    PlotLabel -> Style[f[(a x^2 + b x + c) / (d x + e), 0] , 16, Red]],
  {{a, 2, "a"}, -10, 10}, (*Valori della manipulate*)
  {{b, -7, "b"}, -10, 10},
  {{c, 3, "c"}, -10, 10},
  {{d, 3, "d"}, -10, 10},
  {{e, 2, "e"}, -10, 10},
  {{f, Greater, "f(x)"}, {Greater, GreaterEqual, Less, LessEqual},
    ControlType -> PopupMenu }
];

(*
	Manipulate per visualizzare una NumberLinePlot di una disequazione irrazionale
*)
manipulateDisIrraz[] := Manipulate[
  NumberLinePlot[f[Sqrt[(a x^2 + b x + c)] , 0] , {x, -10, 10},
    PlotLabel -> Style[f[Sqrt[(a x^2 + b x + c)], 0] , 16, Red]],
  {{a, 2, "a"}, -10, 10}, (*Valori della manipulate*)
  {{b, -7, "b"}, -10, 10},
  {{c, 3, "c"}, -10, 10},
  {{f, Greater, "f(x)"}, {Greater, GreaterEqual, Less, LessEqual},
    ControlType -> PopupMenu }
];


(*
	In base allo studio dei segni della disequazione
	Individuare la possibile disequazione giusta
*)
gioco1[] := DynamicModule[{},
(*controllo se la risposta \[EGrave] corretta o sbagliata e visualizzo un dialog con il risultato*)
  checkRisp[result_] :=
      If[result == "Risposta corretta",
        CreateDialog[Column[{Text[Style[StringForm[result], Green, Italic, 24]],
          Button[Style["Chiudi", FontSize -> 18],
            DialogReturn[]
          ]}, ItemSize -> 20]]
        ,
        CreateDialog[Column[{Text[Style[StringForm[result], Red, Italic, 24]],
          Button[Style[
            "Chiudi", FontSize -> 18],
            DialogReturn[]
          ]}, ItemSize -> 20]]
      ];

  ListaRisposte1 = 0;

  ListaRisposte1 = getListaDisequazioni[]; (* Inserisco la lista delle disequazioni in una variabile *)

  Dynamic@ListaRisposte1; (* Serve per aggiornare la variabile ListaRisposte1 *)

  soluz = RandomChoice[ ListaRisposte1];       (* Scelta random tra le varie disequazioni presenti in ListaRisposta1 *)
  Lista2 = DeleteCases[ListaRisposte1, soluz]; (* DeleteCases ritorna tutti gli elementi di ListaRisposta1 meno soluz *)
  risp1 = RandomChoice[ Lista2];               (* Scelta random tra le varie disequazioni presenti in Lista2 *)
  Lista3 = DeleteCases[Lista2, risp1];         (* DeleteCases ritorna tutti gli elementi di Lista2 meno risp1 *)
  risp2 = RandomChoice[ Lista3];               (* Scelta random tra le varie disequazioni presenti in Lista3 *)
  Lista4 = DeleteCases[Lista3, risp2];         (* DeleteCases ritorna tutti gli elementi di Lista3 meno risp2 *)
  risp3 = RandomChoice[ Lista4];               (* Scelta random tra le varie disequazioni presenti in Lista4 *)

  (* Bottoni contenenti le risposte *)
  button1 = Row[{Button["\[FilledCircle]", checkRisp["Risposta sbagliata"], ImageSize -> 30], "    ", risp1 // TraditionalForm}, ImageSize -> 400];
  button2 = Row[{Button["\[FilledCircle]", checkRisp["Risposta sbagliata"], ImageSize -> 30], "    ", risp2 // TraditionalForm}, ImageSize -> 400];
  button3 = Row[{Button["\[FilledCircle]", checkRisp["Risposta sbagliata"], ImageSize -> 30], "    ", risp3 // TraditionalForm}, ImageSize -> 400];
  button4 = Row[{Button["\[FilledCircle]", checkRisp["Risposta corretta"], ImageSize -> 30], "    ", soluz // TraditionalForm}, ImageSize -> 400];

  ListaBottoni = List[button1, button2, button3, button4];


  Row[{Text[Style["                                      Quale disequazione rappresenta lo studio dei segni mostrato?", Blue, Italic, 24]],
    Column[
      {NumberLinePlot[soluz, {x, -20, 20},
        PlotLabel -> Style[
          "\n \!\(\*StyleBox[\"Soluzioni\",\nFontWeight->\"Bold\"]\): "
              <> ToString[TraditionalForm[Reduce[soluz, x]]] "\n" , Gray]
        , ImageSize -> Scaled[.4]]}],
    Column[

      RandomSample[ListaBottoni] (* RandomSample restituisce la stessa lista data in input ma con l'ordine degli elementi cambiato *)
    ]
  }, "      "]
];


(*
	In base alla disequazione mostrata
	Individuare il corretto studio dei segni inerente alla disequazione
*)
gioco2[] := DynamicModule[{},
(*controllo se la risposta \[EGrave] corretta o sbagliata e visualizzo un dialog con il risultato*)
  checkRisp[result_] :=
      If[result == "Risposta corretta",
        CreateDialog[Column[{Text[Style[StringForm[result], Green, Italic, 24]],
          Button[Style["Chiudi", FontSize -> 18],
            DialogReturn[]
          ]}, ItemSize -> 20]]
        ,
        CreateDialog[Column[{Text[Style[StringForm[result], Red, Italic, 24]],
          Button[Style[
            "Chiudi", FontSize -> 18],
            DialogReturn[]
          ]}, ItemSize -> 20]]
      ];

  ListaRisposte2 = 0;

  ListaRisposte2 = getListaDisequazioni[]; (* Inserisco la lista delle disequazioni in una variabile *)

  Dynamic@ListaRisposte2; (* Serve per aggiornare la variabile ListaRisposte2 *)

  soluz = RandomChoice[ ListaRisposte2];       (* Scelta random tra le varie disequazioni presenti in ListaRisposta2 *)
  Lista2 = DeleteCases[ListaRisposte2, soluz]; (* DeleteCases ritorna tutti gli elementi di ListaRisposta1 meno soluz *)
  risp1 = RandomChoice[ Lista2];               (* Scelta random tra le varie disequazioni presenti in Lista2 *)
  Lista3 = DeleteCases[Lista2, risp1];         (* DeleteCases ritorna tutti gli elementi di Lista2 meno risp1 *)
  risp2 = RandomChoice[ Lista3];               (* Scelta random tra le varie disequazioni presenti in Lista3 *)
  Lista4 = DeleteCases[Lista3, risp2];         (* DeleteCases ritorna tutti gli elementi di Lista3 meno risp2 *)
  risp3 = RandomChoice[ Lista4];               (* Scelta random tra le varie disequazioni presenti in Lista4 *)

  (* Bottoni contenenti le risposte *)
  button1 = Row[{Button["\[FilledCircle]", checkRisp["Risposta sbagliata"], ImageSize -> 30], "    ", NumberLinePlot[risp1, {x, -20, 20}, ImageSize -> Scaled[.4], PlotStyle -> Red]}];
  button2 = Row[{Button["\[FilledCircle]", checkRisp["Risposta sbagliata"], ImageSize -> 30], "    ", NumberLinePlot[risp2, {x, -20, 20}, ImageSize -> Scaled[.4], PlotStyle -> Green]}];
  button3 = Row[{Button["\[FilledCircle]", checkRisp["Risposta sbagliata"], ImageSize -> 30], "    ", NumberLinePlot[risp3, {x, -20, 20}, ImageSize -> Scaled[.4], PlotStyle -> Orange]}];
  button4 = Row[{Button["\[FilledCircle]", checkRisp["Risposta corretta"], ImageSize -> 30], "    ", NumberLinePlot[soluz, {x, -20, 20}, ImageSize -> Scaled[.4], PlotStyle -> Blue]}];


  ListaBottoni = List[button1, button2, button3, button4];


  Row[{Text[Style["                                Quale studio dei segni e' soluzione della disequazione mostrata?", Blue, Italic, 24]],
    Column[
      {soluz // TraditionalForm},
      Alignment -> Center,
      ItemSize -> {25, Automatic},
      ItemStyle -> Large],
    Column[
      RandomSample[ListaBottoni](* RandomSample restituisce la stessa lista data in input ma con l'ordine degli elementi cambiato *)

    ]
  }, "      "]
];


(*
	Dato un grafico in input di una equazione
	Individuare il corretto studio dei segni della disequazione

	Con eq >0
*)
gioco3[] := DynamicModule[{},
(*controllo se la risposta \[EGrave] corretta o sbagliata e visualizzo un dialog con il risultato*)
  checkRisp[result_] :=
      If[result == "Risposta corretta",
        CreateDialog[Column[{Text[Style[StringForm[result], Green, Italic, 24]],
          Button[Style["Chiudi", FontSize -> 18],
            DialogReturn[]
          ]}, ItemSize -> 20]]
        ,
        CreateDialog[Column[{Text[Style[StringForm[result], Red, Italic, 24]],
          Button[Style[
            "Chiudi", FontSize -> 18],
            DialogReturn[]
          ]}, ItemSize -> 20]]
      ];

  ListaRisposte3 = 0;

  ListaRisposte3 = getListaEquazioni[]; (* Inserisco la lista delle equazioni in una variabile *)

  Dynamic@ListaRisposte3;   (* Serve per aggiornare la variabile ListaRisposte3 *)

  soluz = RandomChoice[ ListaRisposte3];        (* Scelta random tra le varie disequazioni presenti in ListaRisposta3 *)
  Lista2 = DeleteCases[ListaRisposte3, soluz];  (* DeleteCases ritorna tutti gli elementi di ListaRisposta3 meno soluz *)
  risp1 = RandomChoice[ Lista2];                (* Scelta random tra le varie disequazioni presenti in Lista2 *)
  Lista3 = DeleteCases[Lista2, risp1];          (* DeleteCases ritorna tutti gli elementi di Lista2 meno risp1 *)
  risp2 = RandomChoice[ Lista3];                (* Scelta random tra le varie disequazioni presenti in Lista3 *)
  Lista4 = DeleteCases[Lista3, risp2];          (* DeleteCases ritorna tutti gli elementi di Lista3 meno risp2 *)
  risp3 = RandomChoice[ Lista4];                (* Scelta random tra le varie disequazioni presenti in Lista4 *)

  (* Bottoni contenenti le risposte *)
  button1 = Row[{Button["\[FilledCircle]", checkRisp["Risposta sbagliata"], ImageSize -> 30], "    ", NumberLinePlot[risp1 > 0, {x, -20, 20}, ImageSize -> Scaled[.4], PlotStyle -> Red]}];
  button2 = Row[{Button["\[FilledCircle]", checkRisp["Risposta sbagliata"], ImageSize -> 30], "    ", NumberLinePlot[risp2 > 0, {x, -20, 20}, ImageSize -> Scaled[.4], PlotStyle -> Green]}];
  button3 = Row[{Button["\[FilledCircle]", checkRisp["Risposta sbagliata"], ImageSize -> 30], "    ", NumberLinePlot[risp3 > 0, {x, -20, 20}, ImageSize -> Scaled[.4], PlotStyle -> Orange]}];
  button4 = Row[{Button["\[FilledCircle]", checkRisp["Risposta corretta"], ImageSize -> 30], "    ", NumberLinePlot[soluz > 0, {x, -20, 20}, ImageSize -> Scaled[.4], PlotStyle -> Blue]}];

  ListaBottoni = List[button1, button2, button3, button4];

  quesito = soluz > 0 // TraditionalForm;



  Row[{ Text[Style["                                                              Trovare soluzione per "quesito, Blue, Italic, 24]],
    Column[{

      Plot[soluz, {x, -30, 30},
        Filling -> Axis, FillingStyle -> {RGBColor[0.8, 0.8, 1.0], RGBColor[0.38, 0.51, 0.71]}
        , ImageSize -> Scaled[.4]]
    }],

    Column[

      RandomSample[ListaBottoni](* RandomSample restituisce la stessa lista data in input ma con l'ordine degli elementi cambiato *)
    ]

  }, "      "]
];

(*
Dato un grafico in input di una equazione
Individuare il corretto studio dei segni della disequazione

Con eq >=0
*)
gioco4[] := DynamicModule[{},

(*controllo se la risposta \[EGrave] corretta o sbagliata e visualizzo un dialog con il risultato*)
  checkRisp[result_] :=
      If[result == "Risposta corretta",
        CreateDialog[Column[{Text[Style[StringForm[result], Green, Italic, 24]],
          Button[Style["Chiudi", FontSize -> 18],
            DialogReturn[]
          ]}, ItemSize -> 20]]
        ,
        CreateDialog[Column[{Text[Style[StringForm[result], Red, Italic, 24]],
          Button[Style[
            "Chiudi", FontSize -> 18],
            DialogReturn[]
          ]}, ItemSize -> 20]]
      ];

  ListaRisposte4 = 0;

  ListaRisposte4 = getListaEquazioni[]; (* Inserisco la lista delle equazioni in una variabile *)

  Dynamic@ListaRisposte4;   (* Serve per aggiornare la variabile ListaRisposte4 *)

  soluz = RandomChoice[ ListaRisposte4];        (* Scelta random tra le varie disequazioni presenti in ListaRisposta4 *)
  Lista2 = DeleteCases[ListaRisposte4, soluz];  (* DeleteCases ritorna tutti gli elementi di ListaRisposta4 meno soluz *)
  risp1 = RandomChoice[ Lista2];                (* Scelta random tra le varie disequazioni presenti in Lista2 *)
  Lista3 = DeleteCases[Lista2, risp1];          (* DeleteCases ritorna tutti gli elementi di Lista2 meno risp1 *)
  risp2 = RandomChoice[ Lista3];                (* Scelta random tra le varie disequazioni presenti in Lista3 *)
  Lista4 = DeleteCases[Lista3, risp2];          (* DeleteCases ritorna tutti gli elementi di Lista3 meno risp2 *)
  risp3 = RandomChoice[ Lista4];                (* Scelta random tra le varie disequazioni presenti in Lista4 *)

  (* Bottoni contenenti le risposte *)
  button1 = Row[{Button["\[FilledCircle]", checkRisp["Risposta sbagliata"], ImageSize -> 30], "    ", NumberLinePlot[risp1 >= 0, {x, -20, 20}, ImageSize -> Scaled[.4], PlotStyle -> Red]}];
  button2 = Row[{Button["\[FilledCircle]", checkRisp["Risposta sbagliata"], ImageSize -> 30], "    ", NumberLinePlot[risp2 >= 0, {x, -20, 20}, ImageSize -> Scaled[.4], PlotStyle -> Green]}];
  button3 = Row[{Button["\[FilledCircle]", checkRisp["Risposta sbagliata"], ImageSize -> 30], "    ", NumberLinePlot[risp3 >= 0, {x, -20, 20}, ImageSize -> Scaled[.4], PlotStyle -> Orange]}];
  button4 = Row[{Button["\[FilledCircle]", checkRisp["Risposta corretta"], ImageSize -> 30], "    ", NumberLinePlot[soluz >= 0, {x, -20, 20}, ImageSize -> Scaled[.4], PlotStyle -> Blue]}];

  ListaBottoni = List[button1, button2, button3, button4];

  quesito = soluz >= 0 // TraditionalForm;



  Row[{ Text[Style["                                                              Trovare soluzione per "quesito, Blue, Italic, 24]],
    Column[{

      Plot[soluz, {x, -30, 30},
        Filling -> Axis, FillingStyle -> {RGBColor[0.8, 0.8, 1.0], RGBColor[0.38, 0.51, 0.71]}
        , ImageSize -> Scaled[.4]]
    }],

    Column[

      RandomSample[ListaBottoni](* RandomSample restituisce la stessa lista data in input ma con l'ordine degli elementi cambiato *)
    ]

  }, "      "]
];


(*
Dato un grafico in input di una equazione
Individuare il corretto studio dei segni della disequazione

Con eq <0
*)
gioco5[] := DynamicModule[{},
(*controllo se la risposta \[EGrave] corretta o sbagliata e visualizzo un dialog con il risultato*)
  checkRisp[result_] :=
      If[result == "Risposta corretta",
        CreateDialog[Column[{Text[Style[StringForm[result], Green, Italic, 24]],
          Button[Style["Chiudi", FontSize -> 18],
            DialogReturn[]
          ]}, ItemSize -> 20]]
        ,
        CreateDialog[Column[{Text[Style[StringForm[result], Red, Italic, 24]],
          Button[Style[
            "Chiudi", FontSize -> 18],
            DialogReturn[]
          ]}, ItemSize -> 20]]
      ];

  ListaRisposte5 = 0;

  ListaRisposte5 = getListaEquazioni[]; (* Inserisco la lista delle equazioni in una variabile *)

  Dynamic@ListaRisposte5;   (* Serve per aggiornare la variabile ListaRisposte5 *)

  soluz = RandomChoice[ ListaRisposte5];        (* Scelta random tra le varie disequazioni presenti in ListaRisposta5 *)
  Lista2 = DeleteCases[ListaRisposte5, soluz];  (* DeleteCases ritorna tutti gli elementi di ListaRisposta5 meno soluz *)
  risp1 = RandomChoice[ Lista2];                (* Scelta random tra le varie disequazioni presenti in Lista2 *)
  Lista3 = DeleteCases[Lista2, risp1];          (* DeleteCases ritorna tutti gli elementi di Lista2 meno risp1 *)
  risp2 = RandomChoice[ Lista3];                (* Scelta random tra le varie disequazioni presenti in Lista3 *)
  Lista4 = DeleteCases[Lista3, risp2];          (* DeleteCases ritorna tutti gli elementi di Lista3 meno risp2 *)
  risp3 = RandomChoice[ Lista4];                (* Scelta random tra le varie disequazioni presenti in Lista4 *)

  (* Bottoni contenenti le risposte *)
  button1 = Row[{Button["\[FilledCircle]", checkRisp["Risposta sbagliata"], ImageSize -> 30], "    ", NumberLinePlot[risp1 < 0, {x, -20, 20}, ImageSize -> Scaled[.4], PlotStyle -> Red]}];
  button2 = Row[{Button["\[FilledCircle]", checkRisp["Risposta sbagliata"], ImageSize -> 30], "    ", NumberLinePlot[risp2 < 0, {x, -20, 20}, ImageSize -> Scaled[.4], PlotStyle -> Green]}];
  button3 = Row[{Button["\[FilledCircle]", checkRisp["Risposta sbagliata"], ImageSize -> 30], "    ", NumberLinePlot[risp3 < 0, {x, -20, 20}, ImageSize -> Scaled[.4], PlotStyle -> Orange]}];
  button4 = Row[{Button["\[FilledCircle]", checkRisp["Risposta corretta"], ImageSize -> 30], "    ", NumberLinePlot[soluz < 0, {x, -20, 20}, ImageSize -> Scaled[.4], PlotStyle -> Blue]}];

  ListaBottoni = List[button1, button2, button3, button4];

  quesito = soluz < 0 // TraditionalForm;



  Row[{ Text[Style["                                                              Trovare soluzione per "quesito, Blue, Italic, 24]],
    Column[{

      Plot[soluz, {x, -30, 30},
        Filling -> Axis, FillingStyle -> {RGBColor[0.38, 0.51, 0.71], RGBColor[0.8, 0.8, 1.0]}
        , ImageSize -> Scaled[.4]]
    }],

    Column[

      RandomSample[ListaBottoni](* RandomSample restituisce la stessa lista data in input ma con l'ordine degli elementi cambiato *)
    ]

  }, "      "]
];


(*
Dato un grafico in input di una equazione
Individuare il corretto studio dei segni della disequazione

Con eq <=0
*)
gioco6[] := DynamicModule[{},

(*controllo se la risposta \[EGrave] corretta o sbagliata e visualizzo un dialog con il risultato*)
  checkRisp[result_] :=
      If[result == "Risposta corretta",
        CreateDialog[Column[{Text[Style[StringForm[result], Green, Italic, 24]],
          Button[Style["Chiudi", FontSize -> 18],
            DialogReturn[]
          ]}, ItemSize -> 20]]
        ,
        CreateDialog[Column[{Text[Style[StringForm[result], Red, Italic, 24]],
          Button[Style[
            "Chiudi", FontSize -> 18],
            DialogReturn[]
          ]}, ItemSize -> 20]]
      ];

  ListaRisposte6 = 0;

  ListaRisposte6 = getListaEquazioni[]; (* Inserisco la lista delle equazioni in una variabile *)

  Dynamic@ListaRisposte6;   (* Serve per aggiornare la variabile ListaRisposte6 *)

  soluz = RandomChoice[ ListaRisposte6];        (* Scelta random tra le varie disequazioni presenti in ListaRisposta6 *)
  Lista2 = DeleteCases[ListaRisposte6, soluz];  (* DeleteCases ritorna tutti gli elementi di ListaRisposta6 meno soluz *)
  risp1 = RandomChoice[ Lista2];                (* Scelta random tra le varie disequazioni presenti in Lista2 *)
  Lista3 = DeleteCases[Lista2, risp1];          (* DeleteCases ritorna tutti gli elementi di Lista2 meno risp1 *)
  risp2 = RandomChoice[ Lista3];                (* Scelta random tra le varie disequazioni presenti in Lista3 *)
  Lista4 = DeleteCases[Lista3, risp2];          (* DeleteCases ritorna tutti gli elementi di Lista3 meno risp2 *)
  risp3 = RandomChoice[ Lista4];                (* Scelta random tra le varie disequazioni presenti in Lista4 *)

  (* Bottoni contenenti le risposte *)
  button1 = Row[{Button["\[FilledCircle]", checkRisp["Risposta sbagliata"], ImageSize -> 30], "    ", NumberLinePlot[risp1 <= 0, {x, -20, 20}, ImageSize -> Scaled[.4], PlotStyle -> Red]}];
  button2 = Row[{Button["\[FilledCircle]", checkRisp["Risposta sbagliata"], ImageSize -> 30], "    ", NumberLinePlot[risp2 <= 0, {x, -20, 20}, ImageSize -> Scaled[.4], PlotStyle -> Green]}];
  button3 = Row[{Button["\[FilledCircle]", checkRisp["Risposta sbagliata"], ImageSize -> 30], "    ", NumberLinePlot[risp3 <= 0, {x, -20, 20}, ImageSize -> Scaled[.4], PlotStyle -> Orange]}];
  button4 = Row[{Button["\[FilledCircle]", checkRisp["Risposta corretta"], ImageSize -> 30], "    ", NumberLinePlot[soluz <= 0, {x, -20, 20}, ImageSize -> Scaled[.4], PlotStyle -> Blue]}];


  ListaBottoni = List[button1, button2, button3, button4];

  quesito = soluz <= 0 // TraditionalForm;



  Row[{ Text[Style["                                                              Trovare soluzione per "quesito, Blue, Italic, 24]],
    Column[{

      Plot[soluz, {x, -30, 30},
        Filling -> Axis, FillingStyle -> {RGBColor[0.38, 0.51, 0.71], RGBColor[0.8, 0.8, 1.0]}
        , ImageSize -> Scaled[.4]]
    }],

    Column[

      RandomSample[ListaBottoni](* RandomSample restituisce la stessa lista data in input ma con l'ordine degli elementi cambiato *)
    ]

  }, "      "]
];


(*
	Seleziona e visualizza in maniera random un esercizio tra quelli disponibili
*)
scegliGioco[] := Module[{},

  lista = {gioco1[], gioco2[], gioco3[], gioco4[], gioco5[], gioco6[]}; (* Lista quiz *)
  esercizio = RandomChoice[lista]; (* Scelta random di un elemento di lista *)
  Return[esercizio] (* Ritorna l'esercizio *)

]


(*
  Crea un nuovo bottone per generare un esercizio random dalla lista di esercizi
*)
newExercise[] := DynamicModule[{},

  y = 0;
  Button[
    Text[Style["Nuovo Esercizio", FontSize -> 30, FontColor -> Red]],
    y = scegliGioco[],
    ImageSize -> {250, 80},
    Background -> White]
  Dynamic@y

];

(*
  Ritorna la lista delle equazioni
*)
getListaEquazioni[] := Module[{},
  fun1 = 3x + 3 ;
  fun2 = x^2 + 6x + 1 ;
  fun3 = x^3 + 3x^2 - 3x + 10 ;
  fun4 = x + 2 ;
  fun5 = x^3 + 5x + 4 ;
  fun6 = (x^2 + 3x + 2) / (7x - 3) ;
  fun7 = Sqrt[x^2 + 5x - 3] / (5x - 1) ;
  fun8 = x^4 + 5x^3 + 2x^2 + x - 1 ;
  fun9 = Sqrt[x^2 - x - 2] - 2x + 6;
  fun11 = Sqrt[x^3 + 4x^2 ] ;
  fun13 = x + 3 - (5 / 3 - x);
  fun14 = 2x^3 + x^2 + 4x ;
  fun15 = 3x^2 + x - 6 ;
  fun16 = x^3 + x^2 - x - 7;
  fun18 = (4x + 1) / (x^2 - 2);
  fun21 = (3x^2 - 4) / (x^3 - 1);
  fun22 = (4x^2 - 3 ) / (5x + 1) ;
  fun23 = x - 3 + 5x ;
  fun24 = 2x^2 - x - 6 ;
  fun26 = x^3 + 5x^2 + 4x ;
  fun27 = Sqrt[3x - 1] - 8 ;
  fun30 = 7 - x + Sqrt[5x - 2];

  ListaEquazioni = {fun1, fun2, fun3, fun4, fun5, fun6, fun7, fun8, fun9, fun11, fun13, fun14, fun15, fun16, fun18, fun21, fun22, fun23, fun24, fun26, fun27, fun30};
  Return[ListaEquazioni]
]

(*
  Ritorna lista di disequazioni
*)
getListaDisequazioni[] := Module[{},
  dis1 = 3x + 3 > 0;
  dis2 = x^2 + 6x + 1 < 0;
  dis3 = x^3 + 3x^2 - 3x + 10 >= 0;
  dis4 = x + 2 < 0;
  dis5 = x^3 + 5x + 4 <= 0;
  dis6 = ((x^2 + 3x + 2) / (7x - 3)) > 0;
  dis7 = (Sqrt[x^2 + 5x - 3] / (5x - 1)) >= 0;
  dis9 = Sqrt[x^2 - x - 2] <= 2x + 6;
  dis10 = (4x - 3 ) / 3x + 1 > 0;
  dis11 = Sqrt[x^3 + 4x^2 ] >= 2x - 2;
  dis12 = x^2 - 4 < 0;
  dis13 = x + 3 <= (5 / 3 - x);
  dis14 = x^2 + 4x > 2x^3;
  dis15 = 6 - x < 3x ;
  dis16 = 7 + x^3 - x + x^2 >= 5x + 4;
  dis18 = (4x + 1) / x^2 - 2 > 0;
  dis19 = Sqrt[5x - 3] <= 4;
  dis20 = Sqrt[x - 1] + Sqrt[x + 2] > 0;
  dis21 = (3x^2 - 4) / (x^3 - 1) >= 0;
  dis22 = (4x^2 - 3 ) * (1 + 5x) <= 0;
  dis23 = x - 3 + 5x < 0;
  dis24 = 2x^2 - x - 6 > 0;
  dis25 = Sqrt[1 - x] <= 3x;
  dis26 = 4x + x^3 + 5x^2 >= (x - 1);
  dis27 = Sqrt[3x - 1] - 8 < 0;
  dis28 = 2x + 5 > 6x - 2;
  dis30 = 7 - x < Sqrt[5x - 2];

  ListaDisequazioni = {dis1, dis2, dis3, dis4, dis5, dis6, dis7, dis9, dis10, dis11, dis12, dis13, dis14, dis15, dis16, dis18, dis19, dis20, dis21, dis22, dis23, dis24, dis25, dis26, dis27, dis28, dis30};
  Return[ListaDisequazioni]
]


End[]
(* fine area privata *)

EndPackage[]
(* fine package *) 
