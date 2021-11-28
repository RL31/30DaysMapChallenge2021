library(tidyverse)
library(sf)
library(extrafont)

loadfonts(dev="win")


bleus <- tribble(~fromage,~dpt,~commune,~canton,
                 "Bleu d'Auvergne","12","Brommat, Lacroix-Barrez, Mur-de-Barrez, Murols, Taussac, Thérondels","",
                 "Bleu d'Auvergne","15","Albepierre-Bredons,   Allanche,   Alleuze,   Ally,   Andelat,   Anglards-de-Saint-Flour,Anglards-de-Salers, Antignac, Apchon, Arches, Arnac, Arpajon-sur-Cère, Auriac-l'Église, Aurillac,Auzers,   Ayrens,   Badailhac,   Barriac-les-Bosquets,   Bassignac,   Beaulieu,   Besse,   Boisset,   Bonnac,Brageac,   Brezons,   Calvinet,   Carlat,   Cassaniouze,   Cayrols,   Celles,   Celoux,   Cézens,   Chaliers,Chalinargues, Chalvignac, Champagnac, Champs-sur-Tarentaine-Marchal, Chanterelle, La Chapelle-d'Alagnon, La Chapelle-Laurent, Charmensac, Chastel-sur-Murat, Chaussenac, Chavagnac, Chazelles,Cheylade, Le Claux, Clavières, Collandres, Coltines, Condat, Coren, Crandelles, Cros-de-Montvert,Cros-de-Ronesque, Cussac, Dienne, Drugeac, Escorailles, Le Falgoux, Le Fau, Ferrières-Saint-Mary,Fontanges,   Freix-Anglards,   Giou-de-Mamou,   Girgols,   Glénat,   Gourdièges,   Jaleyrac,   Joursac,   Jou-sous-Monjou,   Junhac,   Jussac,   Labesserette,   Labrousse,   Lacapelle-Barrès,   Lacapelle-del-Fraisse,Lacapelle-Viescamp, Ladinhac, Lafeuillade-en-Vézie, Landeyrat, Lanobre, Lapeyrugue, Laroquebrou,Laroquevieille,   Lascelle,   Lastic,   Laurie,   Lavastrie,   Laveissenet,   Laveissière,   Lavigerie,   Leucamp,Leynhac,   Leyvaux,   Lorcières,   Lugarde,   Madic,   Malbo,   Mandailles-Saint-Julien,   Marcenat,Marchastel, Marcolès, Marmanhac, Massiac, Mauriac, Maurs, Méallet, Menet, Mentières, Molèdes,Molompize, La Monselie, Montboudif, Montchamp, Le Monteil, Montgreleix, Montsalvy, Montvert,Mourjou, Moussages, Murat, Narnhac, Naucelles, Neussargues-Moissac, Neuvéglise, Nieudan, Omps,Oradour,   Pailherols,   Parlan,   Paulhac,   Paulhenc,   Peyrusse,   Pierrefort,   Pleaux,   Polminhac,   Pradiers,Prunet, Quézac, Rageade, Raulhac, Reilhac, Rézentières, Riom-ès-Montagnes, Roannes-Saint-Mary,Roffiac,   Rouffiac,   Le   Rouget-Pers,   Roumégoux,   Rouziers,   Ruynes-en-Margeride,   Saignes,   Saint-Amandin,   Saint-Antoine,   Saint-Bonnet-de-Condat,   Saint-Bonnet-de-Salers,   Saint-Cernin,   Saint-Chamant,   Saint-Cirgues-de-Jordanne,   Saint-Cirgues-de-Malbert,   Saint-Clément,   Saint-Constant-Fournoulès, Saint-Étienne-Cantalès, Saint-Étienne-de-Carlat, Saint-Étienne-de-Maurs, Saint-Étienne-de-Chomeil,   Saint-Flour,   Saint-Georges,   Saint-Gérons,   Saint-Hippolyte,   Saint-Illide,   Saint-Jacques-des-Blats,   Saint-Julien-de-Toursac,   Saint-Mamet-la-Salvetat,   Saint-Martin-Cantalès,   Saint-Martin-sous-Vigouroux, Saint-Martin-Valmeroux, Saint-Mary-le-Plain, Saint-Paul-des-Landes, Saint-Paul-de-Salers, Saint-Pierre, Saint-Poncy, Saint-Projet-de-Salers, Saint-Santin-Cantalès, Saint-Saturnin, Saint-Saury,  Saint-Simon,  Saint-Victor, Saint-Vincent-de-Salers, Sainte-Anastasie, Sainte-Eulalie, Sainte-Marie,   Salers,   Salins,   Sansac-de-Marmiesse,   Sansac-Veinazès,   Sauvat,   La   Ségalassière,   Ségur-les-Villas,   Sénezergues,   Sériers,   Siran,   Soulages,   Sourniac,   Talizat,   Tanavelle,   Teissières-de-Cornet,Teissières-lès-Bouliès, Les Ternes, Thiézac, Tiviers, Tournemire, Trémouille, Trizac, Ussel, Vabres,Val   d’Arcomie,   Valette,   Valjouze,   Valuéjols,   Le   Vaulmier,   Vebret,   Védrines-Saint-Loup,   Velzic,Vernols, Veyrières, Vézac, Vèze, Vezels-Roussy, Vic-sur-Cère, Vieillespesse, Vieillevie, Le Vigean,Villedieu, Virargues, Vitrac, Ydes, Yolet, Ytrac","",
                 "Bleu d'Auvergne","19","Altillac, Auriac, Bassignac-le-Bas, Bassignac-le-Haut, Bort-les-Orgues, Camps-Saint-Mathurin-Léobazel,   La   Chapelle-Saint-Géraud,   Confolent-Port-Dieu,   Darazac,   Eygurande,   Feyt,Goulles,   Hautefage,   Lafage-sur-Sombre,   Lamazière-Basse,   Lapleau,   Laroche-près-Feyt,   Latronche,Laval-sur-Luzège, Liginiac, Margerides, Mercœur, Merlines, Mestes, Monestier-Merlines, Monestier-Port-Dieu,   Neuvic,   Palisse,   Reygade,   Rilhac-Xaintrie,   Saint-Bonnet-les-Tours-de-Merle,   Saint-Bonnet-près-Bort, Saint-Cirgues-la-Loutre, Saint-Étienne-aux-Clos, Saint-Exupéry-les-Roches, Saint-Fréjoux, Saint-Geniez-ô-Merle, Saint-Hilaire-Foissac, Saint-Hilaire-Luc, Saint-Julien-aux-Bois, Saint-Julien-le-Pèlerin,   Saint-Julien-près-Bort,   Saint-Pantaléon-de-Lapleau,   Saint-Privat,   Saint-Victour,Sarroux, Sérandon, Servières-le-Château, Sexcles, Soursac, Thalamy, Ussel, Veyrières","",
                 "Bleu d'Auvergne","43","Ally,   Arlet,   Autrac,   Auvers,   La   Besseyre-Saint-Mary,   Blesle,   Bonneval,   La   Chaise-Dieu,   La   Chapelle-Geneste,   Charraix,   Chastel,   Chazelles,   Cistrières,   Cronce,   Desges,   Espalem,Ferrussac,   Grenier-Montgon,   Lubilhac,   Malvières,   Mercœur,   Pébrac,   Pinols,   Saint-Austremoine,Saint-Cirgues, Saint-Étienne-sur-Blesle, Sembadel, Tailhac","",
                 "Bleu d'Auvergne","46","Anglars,   Bannes,   Bessonies,   Le   Bourg,   Le   Bouyssou,   Cahus,   Cardaillac,   Cornac,Espeyroux, Frayssinhes, Gorses, Labastide-du-Haut-Mont, Labathude, Lacapelle-Marival, Latouille-Lentillac, Latronquière, Lauresses, Laval-de-Cère, Leyme, Molières, Montet-et-Bouxal, Prendeignes,Sabadel-Latronquière,   Saint-Bressou,   Saint-Cirgues,   Saint-Hilaire,   Saint-Maurice-en-Quercy,   Saint-Médard-Nicourby, Saint-Perdoux, Saint-Vincent-du-Pendit, Sainte-Colombe, Sénaillac-Latronquière,Sousceyrac-en-Quercy, Terrou, Teyssieu, Viazac","",
                 "Bleu d'Auvergne","48","Albaret-le-Comtal,  Albaret-Sainte-Marie,  Arzenc-d'Apcher,  Aumont-Aubrac,  Auroux,Les Bessons, Blavignac, Brion, Chambon-le-Château, Chastanier, Chauchailles, Chaulhac, La Chaze-de-Peyre, Cheylard-l'Évêque, Estables, La Fage-Montivernoux, La Fage-Saint-Julien, Fau-de-Peyre,Fontans, Fournels, Grandrieu, Javols, Julianges, Lachamp, Lajo, Langogne, Les Laubies, Laval-Atger,Luc,   Le   Malzieu-Forain,   Le   Malzieu-Ville,   Les   Monts-Verts,   Naussac-Fontanes,   Noalhac,   La Panouse,   Paulhac-en-Margeride,   Prunières,   Ribennes,   Rieutort-de-Randon,   Rimeize,   Rocles,   Saint-Alban-sur-Limagnole,   Saint-Amans,   Saint-Bonnet-de-Montauroux,   Saint-Chély-d'Apcher,   Saint-Denis-en-Margeride, Saint-Flour-de-Mercoire, Saint-Gal, Saint-Juéry, Saint-Laurent-de-Veyrès, Saint-Léger-du-Malzieu, Saint-Paul-le-Froid, Saint-Pierre-le-Vieux, Saint-Privat-du-Fau, Saint-Sauveur-de-Peyre, Saint-Symphorien, Sainte-Colombe-de-Peyre, Sainte-Eulalie, Serverette, Servières, Termes, La Villedieu","",
                 "Bleu d'Auvergne","63","Aix-la-Fayette,   Ambert,   Les   Ancizes-Comps,   Anzat-le-Luguet,   Apchat,   Arlanc,Augerolles,   Aurières,   Auzelles,   Avèze,   Baffie,   Bagnols,   Bertignat,   Besse-et-Saint-Anastaise,Beurières, Bongheat, Bort-l'Étang, La Bourboule, Bourg-Lastic, Briffons, Bromont-Lamothe, Brousse,Bulhon,   La   Celle,   Ceilloux,   Ceyssat,   Chambon-sur-Dolore,   Chambon-sur-Lac,   Chaméane,Champagnat-le-Jeune, Champétières, Chapdes-Beaufort, La Chapelle-Agnon, La Chapelle-sur-Usson,Charbonnières-les-Vieilles,   Charensat,   Charnat,   Chastreix,   Chaumont-le-Bourg,   Cisternes-la-Forêt,Combrailles,   Compains,   Condat-en-Combraille,   Condat-lès-Montboissier,   Courpière,   Crevant-Laveine, Cros, Culhat, Cunlhat, Domaize, Doranges, Dorat, Dore-l'Église, Échandelys, Égliseneuve-d'Entraigues, Égliseneuve-des-Liards, Églisolles, Escoutoux, Espinchal, Estandeuil, Esteil, Fayet-le-Château,   Fayet-Ronaye,   Fernoël,   La   Forie,   Fournols,   Gelles,   Giat,   La   Godivelle,   La   Goutelle,Grandrif,   Grandval,   Herment,   Heume-l'Église,   Isserteaux,   Job,   Jumeaux,   Labessette,   Landogne,Laqueuille,   Larodde,   Lastic,   Lezoux,   Limons,   Luzillat,   Manglieu,   Manzat,   Marat,   Marsac-en-Livradois,   Mauzun,   Mayres,   Mazaye,   Mazoires,   Medeyrolles,   Messeix,   Miremont,   Le   Monestier,Mons,   Mont-Dore,   Montel-de-Gelat,   Montfermy,   Montmorin,   Murat-le-Quaire,   Murol,   Nébouzat,Néronde-sur-Dore, Neuville, Noalhat, Novacelles, Olby, Olliergues, Olmet, Orcival, Orléat, Paslières,Perpezat, Peschadoires, Peslières, Picherande, Pontaumur, Pontgibaud, Prondines, Pulvérières, Puy-Saint-Gulmier,   Queuille,   Roche-Charles-la-Mayrand,   Rochefort-Montagne,   Saillant,   Saint-Alyre-d'Arlanc,   Saint-Alyre-ès-Montagne,   Saint-Amant-Roche-Savine,   Saint-Anthème,   Saint-Avit,   Saint-Bonnet-le-Bourg,   Saint-Bonnet-le-Chastel,   Saint-Bonnet-près-Orcival,   Saint-Clément-de-Valorgue,Saint-Dier-d'Auvergne,   Saint-Donat,   Saint-Éloy-la-Glacière,   Saint-Étienne-des-Champs,   Saint-Étienne-sur-Usson,   Saint-Ferréol-des-Côtes,   Saint-Flour,   Saint-Genès-Champanelle,   Saint-Genès-Champespe,   Saint-Genès-la-Tourette,   Saint-Georges-de-Mons,   Saint-Germain-près-Herment,   Saint-Germain-l'Herm,   Saint-Gervais-sous-Meymont,   Saint-Hilaire-les-Monges,   Saint-Jacques-d'Ambur,Saint-Jean-d'Heurs,   Saint-Jean-des-Ollières,   Saint-Jean-en-Val,   Saint-Jean-Saint-Gervais,   Saint-Julien-Puy-Lavèze, Saint-Just, Saint-Martin-des-Olmes, Saint-Martin-d'Ollières, Saint-Nectaire, Saint-Ours,   Saint-Pierre-Colamine,   Saint-Pierre-le-Chastel,   Saint-Pierre-Roche,   Saint-Priest-des-Champs,Saint-Quentin-sur-Sauxillanges,   Saint-Romain,   Saint-Sauves-d'Auvergne,   Saint-Sauveur-la-Sagne,Saint-Sulpice,   Saint-Victor-la-Rivière,   Sainte-Agathe,   Sainte-Catherine,   Sallèdes,   Saulzet-le-Froid,Sauvagnat, Sauvessanges, Sauviat, Sauxillanges, Savennes, Sermentizon, Singles, Sugères, Tauves,Thiers,   Thiolières,   Tortebesse,   La   Tour-d'Auvergne,   Tours-sur-Meymont,Tralaigues,   Trémouille-Saint-Loup,   Trézioux,   Valbeleix,   Valz-sous-Châteauneuf,   Vernet-la-Varenne,   Le   Vernet-Sainte-Marguerite, Verneugheol, Vernines, Vertolaye, Villosanges, Vinzelles, Viverols, Voingt","",
                 "Bleu de Gex","01","Gex, Lélex, Mijoux, Chèzery-Forens, Confort, Lancrans, Léaz, Champfromier, Giron, Montanges, Plagne, Saint-Germain-de-Joux, Grand-Abergement, Petit-Abergement, Belleydoux, Échallon, Apremont, Charix, Lalleyriat, Le Poizat,Crozet, Échenevex, Vesancy, Divonne-les-Bains, Péron, Farges, Collonges, Saint-Jean-de-Gonville, Thoiry, Sergy, Billiat, Châtillon-en-Michaille, Injoux-Génissiat,Villes,Bellegarde-sur-Valserine","",
                 "Bleu de Gex","39","Bellecombe, Les Bouchoux, Choux, Coiserette, Coyriere, Larrivoire, Les Moussières, La Pesse, Rogna, Viry, Vulvoz, Lézat, Longchaumois, La Mouille, Prémanon, Tancua, Chassal,Lajoux, Lamoura, Lavancia-Epercy, Molinges, Les Molunes, La Rixouse, Saint-Claude, Septmoncel, Vaux-lès-Saint-Claude, Villard-Saint-Sauveur, Villard-sur-Bienne, Château-des-Prés, La Chaumusse, Chaux-des-Prés, La Chaux-du-Dombief, Fort-du-Plasne, Grande-Rivière, Lac-des-Rouges-Truites, Les Piards, Prénovel, Saint-Laurent-en-Grandvaux, Saint-Maurice-Crillat, Saint-Pierre","",
                 "Bleu des Causses","12 et 81","Campouriez, Cassuéjouls, Condom-d'Aubrac, Curières, Florentin-la-Capelle, Huparlac, Laguiole, Montézic, Montpeyroux, Saint-Amans-des-Cots, Saint-Chély-d'Aubrac, Saint-Symphorien-de-Thénières, Soulages-Bonneval","Causse-Comtal, Causses-Rougiers, Ceor-Ségala, Enne and Alzou, Lot and Dourdou, Lot and Montbazinois, Lot and Palanges, Lot and Truyère, Millau-1, Millau-2, Monts du Réquistanais, Nord-Lévezou, Raspes and Lévezou, Rodez-1, Rodez-2, Rodez-Onet, Saint-Affrique, Tarn and Causses, Vallon, Villefranche-de-Rouergue, Villeneuvois and Villefranchois",
                 "Bleu des Causses","30","Trèves","",
                 "Bleu des Causses","34","Pégairolles-de-l'Escalette","",
                 "Bleu des Causses","46","Boissières, Le Boulvé, Boussac, Calamane, Calès, Cambes, Cassagnes, Catus, Cœur-de-Causse, Corn, Crayssac, Duravel, Durbans, Espère, Flaujac-Gare, Floressas, Francoulès, Gignac, Gigouzac, Ginouillac, Grézels, Les Junies, Labastide-du-Vert, Lacapelle-Cabanac, Lachapelle-Auzac, Lagardelle, Lamothe-Cassel, Lamothe-Fénelon, Lanzac, Lherm, Livernon, Loupiac, Mauroux, Maxou, Mechmont, Montamel, Montcabrier, Montfaucon, Montgesty, Nadaillac-de-Rouge, Nuzéjouls, Payrac, Pescadoires, Pontcirq, Prayssac, Puy-l\'Évêque, Reilhac, Reilhaguet, Le Roc, Saint-Chamarand, Saint-Cirq-Souillaguet, Saint-Denis-Catus, Saint-Martin-le-Redon, Saint-Matré, Saint-Médard, Saint-Pierre-Lafeuille, Saint-Projet, Saux, Séniergues, Sérignac, Soturac, Soucirac, Souillac, Touzac, Ussel, Uzech, Le Vigan, Vire-sur-Lot","Cahors-1, Cahors-2, Cahors-3, Causse and Vallées, Luzech, Marches du Sud-Quercy",
                 "Bleu des Causses","48","Allenc, Antrenas, Aumont-Aubrac, Badaroux, Les Bessons, Brenoux, Le Buisson, Chadenet, Chaulhac, La Chaze-de-Peyre, La Fage-Saint-Julien, Fau-de-Peyre, Fraissinet-de-Fourques, Gatuzières, Hures-la-Parade, Ispagnac, Javols, Julianges, Lanuéjols, Le Malzieu-Forain, Le Malzieu-Ville, Marvejols, Mas-Saint-Chély, Mende, Meyrueis, Montbrun, Les Monts-Verts, Paulhac-en-Margeride, Quézac, Recoules-de-Fumas, Le Rozier, Saint-Bauzile, Saint-Étienne-du-Valdonnez, Saint-Laurent-de-Muret, Saint-Léger-de-Peyre, Saint-Léger-du-Malzieu, Saint-Pierre-de-Nogaret, Saint-Pierre-des-Tripiers, Saint-Privat-du-Fau, Saint-Sauveur-de-Peyre, Sainte-Colombe-de-Peyre, Sainte-Hélène, Vebron,Cans-et-Cévennes,Florac-Trois-Rivières","La Canourgue, Chirac, Saint-Chély-d'Apcher",
                 "Bleu du Vercors-Sassenage","26","Le Chaffal, La Chapelle-en-Vercors, Echevis, Léoncel, Omblèze, Plan-de-Baix, Saint-Agnan-en-Vercors, Saint-Julien-en-Vercors, Saint-Martin-en-Vercors, Vassieux-en-Vercors,Bouvante, Saint-Jean-en-Royans, Saint-Laurent-en-Royans","",                 "Bleu du Vercors-Sassenage","38","Autrans-Méaudre-en-Vercors, Chatelus, Choranche, Corrençon-en-Vercors, Engins, Lans-en-Vercors, Malleval-en-Vercors, Presles, Rencurel, Saint-Nizier-du-Moucherotte, Villard-de-Lans,Izeron, Saint-Pierre-de-Chérennes","",
                 "Fourme d\'Ambert","63","Aydat, Bansat, Blot-l’Eglise, Bongheat, Chaméane, Champagnat-le-Jeune, Chanat-la-Mouteyre, Chanonat, La Chapelle-sur-Usson, Châteldon, Châtelguyon, Clémensat, Combronde, Courgoul, Cournols, Creste, Durtol, Egliseneuve-des-Liards, Enval, Esteil, Grandeyrolles, Isserteaux, Lachaux, Lisseuil, Ludesse, Manglieu, Mauzun, Menat, Montaigut-le-Blanc, Montmorin, Néronde-sur-Dore, Neuf-Eglise, Olloix, Orcines, Orléat, Paslières, Peschadoires, Peslières, Pignols, Pouzol, Puy-Guillaume, Ris, Romagnat, Saint-Etienne-sur-Usson, Saint-Floret, Saint-Gal-sur-Sioule, Saint-Genès-Champanelle, Saint-Genès-la-Tourette, Saint-Gervazy, Saint-Jean-en-Val, Saint-Jean-Saint-Gervais, Saint-Martin-d’Ollières, Saint-Nectaire, Saint-Pardoux, Saint-Quentin-sur-Sauxillanges, Saint-Rémy-de-Blot, Saint-Sandoux, Saint-Saturnin, Saint-Vincent, Sallèdes, Saulzet-le-Froid, Saurier, Sauxillanges, Sayat, Servant, Sugères, Teilhet, Tourzel-Ronzières, Valz-sous-Châteauneuf, Vernet-la-Varenne, Le Vernet-Sainte-Marguerite, Verrières, Vodable, Volvic","Ambert, Ardes, Arlanc, Besse-et-Saint-Anastaise, Bourg-Lastic, Courpière, Cunlhat, Herment, Manzat, Montaigut, Olliergues, Pionsat, Pontaumur, Pontgibaud, Rochefort-Montagne, Saint-Amant-Roche-Savine, Saint-Anthème, Saint-Dier-d’Auvergne, Saint-Germain-l’Herm, Saint-Gervais-d’Auvergne, Saint-Rémy-sur-Durolle, Tauves, Thiers, La Tour-d’Auvergne, Viverols",
                 "Fourme d\'Ambert","15","","Allanche, Condat, Murat, Saint-Flour – Nord, Saint-Flour – Sud",
                 "Fourme d\'Ambert","42","Chalmazel, La Chamba, La Chambonie, Jeansagnière, Lérigneux, Roche, Saint-Bonnet-le-Courreau, Sauvain","",
                 "Fourme de Montbrison","42","Bard, Cervières, Chalmazel, La Chamba, La Chambonie, Champoly, Châtelneuf, La Côte-en-Couzan, Ecotay-l’Olme, Essertines-en-Châtelneuf, Jeansagnière, Lérigneux, Noirétable, Palogneux, Roche, Saint-Bonnet-le-Courreau, Saint-Didier-sur-Rochefort, Saint-Georges-en-Couzan, Saint-Jean-la-Vêtre, Saint-Julien-la-Vêtre, Saint-Just-en-Bas, Saint-Laurent-Rochefort, Saint-Priest-la-Vêtre, Saint-Romain-d’Urfé, Les Salles, Sauvain, La Valla, Verrières-en-Forez","",
                 "Fourme de Montbrison","63","Brugeron, Job, Saint-Anthème, Saint-Pierre-la-Bourlhonne,Valcivières","",
                 "Roquefort","11","Belpech, Brousses-et-Villaret, Castans, Caudebronde, Cenne-Monestiés, Cuxac-Cabardès, Fanjeaux , Fontiers-Cabardès, Fraisse-Cabardès, Labastide-Esparbairenque, Lacombe, Laprade, Lespinassière, Les Martys, Mas-Cabardès, Mayreville, Miraval-Cabardès, Montolieu, Pradelles-Cabardès, Roquefère, Saint-Denis, Saissac, La Tourette-Cabardès, Verdun-en-Lauragais, Villardonnel, Villemagne","",
                 "Roquefort","12","Les Albres, Anglars-Saint-Félix, Asprières, Auzits, Le Bas Ségala, Belcastel, Bertholène, Bessuéjouls, Bor-et-Bar, Bournazel, Brandonnet, La Capelle-Bleys, Castelmary, Castelnau-de-Mandailles , Compolibat, Conques-en-Rouergue , Crespin, Drulhe, Escandolières, Espalion, La Fouillade, Gaillac-d'Aveyron, Galgan, Goutrens, Laissac-Sévérac l'Église, Lanuéjouls, Lassouts, Lescure-Jaoul, Lugan, Lunac, Maleville, Mayran, Millau, Montbazens, Morlhon-le-Haut, Najac, Palmas d'Aveyron, Peyrusse-le-Roc, Pierrefiche, Pomayrols, Prades-d'Aubrac , Prévinquières, Privezac, Rieupeyroux, Rignac, Rodez, Roussennac, Saint-André-de-Najac, Saint-Côme-d'Olt, Saint Geniez d'Olt et d'Aubrac, Sainte-Eulalie-d'Olt, La Salvetat-Peyralès, Sanvensa, Sébrazac, Sonnac, Tayrac, Valzergues, Vaureilles, Villecomtal, Vimenet","Causse-Comtal, Causses-Rougiers, Ceor-Ségala, Millau-1, Millau-2, Monts du Réquistanais, Nord-Lévezou, Raspes et Lévezou, Rodez-2, Rodez-Onet, Vallon, Saint-Affrique, Tarn and Causses",
                 "Roquefort","30","Alzon, Blandas, Campestre-et-Luc, Causse-Bégon, Dourbies, Lanuéjols, Montdardier, Revens, Rogues, Saint-Sauveur-Camprieu, Trèves, Vissec","",
                 "Roquefort","34","Les Aires, Avène, Bédarieux, Le Bousquet-d'Orb, Brenas, Cambon-et-Salvergues, Camplong, Carlencas-et-Levas, Cassagnoles, Castanet-le-Haut, Le Caylar, Ceilhes-et-Rocozels, Colombières-sur-Orb, Combes, Courniou, Le Cros, Dio-et-Valquières, Ferrals-les-Montagnes, Fraisse-sur-Agout, Graissessac, Hérépian, Joncels, Lamalou-les-Bains, Lauroux, Lavalette, Liausson, Lodève, Lunas, Mérifons, Mons, Mourèze, Octon, Olargues, Olmet-et-Villecun, Pégairolles-de-l'Escalette, Pézènes-les-Mines, Les Plans, Le Poujol-sur-Orb, Le Pradal, Prémian, Le Puech, Riols, Les Rives, Romiguières, Roqueredonde, Rosis, Saint-Étienne-d'Albagnan, Saint-Étienne-Estréchoux, Saint-Félix-de-l'Héras, Saint-Geniès-de-Varensal, Saint-Gervais-sur-Mare, Saint-Julien, Saint-Martin-de-l'Arçon, Saint-Maurice-Navacelles, Saint-Michel, Saint-Pierre-de-la-Fage, Saint-Pons-de-Thomières, Saint-Vincent-d'Olargues, Salasc, La Salvetat-sur-Agout, Sorbs, Le Soulié, Taussac-la-Billière, La Tour-sur-Orb, La Vacquerie-et-Saint-Martin-de-Castries, Valmascle, Verreries-de-Moussans, Vieussan, Villemagne-l'Argentière","",
                 "Roquefort","48","Allenc, Badaroux, Banassac-Canilhac, Les Bondons, Brenoux, La Canourgue, Cans et Cévennes, Chadenet, Chanac, Florac Trois Rivières , Fraissinet-de-Fourques, Gatuzières, Gorges du Tarn Causses, Les Hermaux, Hures-la-Parade, Ispagnac, Lachamp, Lanuéjols, Laval-du-Tarn, La Malène, Marvejols, Mas-Saint-Chély, Massegros Causses Gorges, Mende, Meyrueis, Le Rozier, Saint-Bauzile, Saint-Étienne-du-Valdonnez, Saint-Pierre-de-Nogaret, Saint-Pierre-des-Tripiers, Saint-Saturnin, Sainte-Hélène, Servières, La Tieule, Trélans, Vebron","Chirac",
                 "Roquefort","81","Alban, Amarens, Ambialet, Arifat, Arthès, Bellegarde-Marsal, Bernac, Brousse, Burlats, Cagnac-les-Mines, Carmaux, Castanet, Castelnau-de-Lévis , Castres, Cestayrols , Cordes-sur-Ciel , Curvalle, Dénat , Fauch, Le Fraysse, Le Garric, Graulhet , Labessière-Candeil, Lacapelle-Ségalar, Laparrouquial, Lasgraisses , Lautrec, Lempaut , Lescure-d'Albigeois, Lombers, Mailhoc, Massals, Mazamet, Miolles, Monestiés, Montfa, Montirat, Montredon-Labessonnié, Mont-Roc, Mouzens , Mouzieys-Panens , Navès , Paulinet, Peyregoux , Poulan-Pouzols, Pratviel , Puéchoursi , Puylaurens, Rayssac, Réalmont, Ronel, Roquecourbe, Rouffiac, Roumégoux, Saint-André, Saint-Antonin-de-Lacalm, Saint-Christophe, Saint-Jean-de-Vals, Saint-Julien-du-Puy, Saint-Lieux-Lafenasse, Saint-Marcel-Campes, Saint-Martin-Laguépie, Saint-Salvy-de-la-Balme, Sainte-Croix, Salles, Le Ségur, Sieurac, Soual, Souel , Técou , Teillet, Terre-Clapier, Le Travet, Trévien, Vénès, Villefranche-d'Albigeois, Viviers-lès-Montagnes","Carmaux-1 Le Ségala, Les Hautes Terres d'Oc, Mazamet-1, Mazamet-2 Vallée du Thoré, La Montagne noire",
)

bleus_communes <- bleus %>% 
  select(-canton) %>% 
  separate_rows(commune,sep=",") %>% 
  mutate(commune=trimws(commune,"both")) %>% 
  filter(commune!="")

bleus_cantons <- bleus %>% 
  select(-commune) %>% 
  separate_rows(canton,sep=",") %>% 
  mutate(canton=trimws(canton,"both")) %>% 
  filter(canton!="")

communes <- st_read("donnees/COMMUNE.shp")

communes_appariees <- communes %>% 
  select(INSEE_DEP,NOM_COM) %>% 
  mutate(NOM_COM=str_replace_all(NOM_COM," ","")) %>% 
  inner_join(bleus_communes %>% 
  mutate(commune=str_replace_all(commune," ","")),
             by=c("INSEE_DEP"="dpt","NOM_COM"="commune")) %>% 
  group_by(fromage) %>% 
  summarise()

communes_appariees %>% st_drop_geometry() %>% as.data.frame() %>% view()

cantons <- st_read("donnees/CANTON_FRANCE_region.shp",options="ENCODING=Latin1")

cantons_apparies <- cantons %>% 
  select(DEP,NOMCT) %>% 
  mutate(NOMCT=str_replace_all(NOMCT," ","")) %>% 
  right_join(bleus_cantons %>% 
               mutate(canton=str_replace_all(canton," ","")),
             by=c("DEP"="dpt","NOMCT"="canton")) %>% 
  group_by(fromage) %>% 
  summarise()

ok <- bleus_cantons %>% 
  mutate(canton=str_replace_all(canton," ",""),
         dpt=substr(dpt,1,2)) %>% 
  inner_join(cantons %>% 
               select(DEP,NOMCT) %>% 
               mutate(NOMCT=str_replace_all(NOMCT," ","")),
             by=c("dpt"="DEP","canton"="NOMCT")) %>% select(-geometry) %>% view()

# 
# bind_rows(bleus_cantons %>% 
#             mutate(canton=str_replace_all(canton," ",""),
#                    dpt=substr(dpt,7,8)) %>% 
#             inner_join(cantons %>% 
#                          select(DEP,NOMCT) %>% 
#                          mutate(NOMCT=str_replace_all(NOMCT," ","")),
#                        by=c("dpt"="DEP","canton"="NOMCT")) %>% select(-geometry) %>% view()
# 

bleus_cantons %>% 
  anti_join(ok,by="canton")



bleus_sf <- communes_appariees %>% 
  bind_rows(st_transform(cantons_apparies,crs=2154)) %>% 
  group_by(fromage) %>% 
  summarise()

FR <- st_read("donnees/REGION.shp") %>% summarise()
bleus_sf %>% 
  ggplot()+
  geom_sf(data = FR, fill=NA, color="white",alpha=.1)+
  geom_sf(fill="aquamarine4",color=NA,alpha=.9)+
  facet_wrap(~fromage)+
  labs(     caption="Sources : Commission européenne, Registre des produits de qualité\nIGN, Admin Express COG\nTraitements et erreurs : Re_Mi_La")+
  theme_void()+
  theme(plot.title.position = "plot",
        plot.title = element_text(face="bold",size=20),
        plot.caption = element_text(face="italic",size = 8),
        text = element_text(colour = "aquamarine4",family = "Calibri"),
        plot.background = element_rect(fill="gray93",color="gray93"))

carte_bleu <- function(FROMAGE){
  carte <- bleus_sf %>% 
    filter(fromage==FROMAGE) %>% 
    ggplot(data=.)+
    geom_sf(data = FR, fill=NA, color="white",alpha=.1)+
    geom_sf(fill="aquamarine4",color=NA,alpha=.9)+
#    labs(     caption="Sources : Commission européenne, Registre des produits de qualité\nIGN, Admin Express COG\nTraitements et erreurs : Re_Mi_La")+
    theme_void()+
    theme(plot.title.position = "plot",
          plot.title = element_text(face="bold",size=20),
          plot.caption = element_text(face="italic",size = 8),
          text = element_text(colour = "aquamarine4",family = "Calibri"),
          plot.background = element_rect(fill="gray93",color="gray93"))  
  
  saveRDS(carte,paste0("sorties/",str_replace_all(FROMAGE," ","_"),".RDS"))
  
}


liste <- bleus %>% select(fromage) %>% distinct() %>% pull()



carte_bleu("Bleu de Gex")
walk(liste,carte_bleu)

gex <- readRDS("sorties/Bleu_de_Gex.RDS")
roquefort <- readRDS("sorties/Roquefort.RDS")
montbrison <- readRDS("sorties/Fourme_de_Montbrison.RDS")
ambert <- readRDS("sorties/Fourme_d'Ambert.RDS")
vercors <- readRDS("sorties/Bleu_du_Vercors-Sassenage.RDS")
auvergne <- readRDS("sorties/Bleu_d'Auvergne.RDS")
causses <- readRDS("sorties/Bleu_des_Causses.RDS")


library(gridExtra)
library(grid)
library(lattice)
# p <- qplot(1,1)
# p2 <- xyplot(1~1)
# r <- rectGrob(gp=gpar(fill="grey90"))
t <- textGrob("text")
grid.arrange(t, gex, gex, gex, ncol=2)


base_ok %>% count(dpt)


ggplot(x=30,y=20,
       text="titre",colour="black",size=10)+
geom_text()+
  theme_void()
 


titre <- ggplot() + 
  annotate("text", x = 2, y = 25, size=6, 
           label = "Aire géographiquesTerritoires de production\ndu lait des bleus\navec AOP",
           colour="aquamarine4",
           hjust=0) + 
  lims(x=c(2,10))+
  theme_void()+
  theme(plot.title.position = "plot",
        plot.title = element_text(face="bold",size=20),
        plot.caption = element_text(face="italic",size = 8),
        text = element_text(colour = "aquamarine4",family = "Calibri"),
        plot.background = element_rect(fill="gray93",color="gray93")) 

library(patchwork)
(auvergne|montbrison|ambert)/(titre|roquefort)/(vercors|gex|causses)
a <- (auvergne|montbrison|ambert)
b <- (titre|roquefort)
c <- (vercors|gex|causses)


d <- (auvergne|montbrison)/titre
e <- ambert/roquefort

d|e

a/b/c+
  plot_layout() &
  theme(plot.background = element_rect(color  = 'gray93', fill ="gray93"))


