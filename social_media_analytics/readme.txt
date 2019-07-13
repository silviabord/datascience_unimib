DATA
* nella cartella en si trovano un file per ogni giorno monitorato con i tweet con keyword #Cannes2019 (tramite codice 01_download_cannes.R)
* nella cartella topic si trovano i tweet con keyword relative a fashion o cinema utilizati per il modello di topic detection (tramite codice 03_download_topic.R)

SCRIPTS
01_download_cannes.R
02_clean_cannes.R
03_download_topic.R
04_topic_model.R
05_network_analytics.R
06_social_analytics.R

TABLEAU
contiene il file tableau con cui sono stati fatti tutti i grafici del progetto e tutti i diversi file di input provenienti da script R

GEPHI
contiene il file .gephi e tre diversi file di input
* gephi_link_w contiene tutte le relazioni tra nodi tramite retweet con peso pari al numero di volte in cui si verifica la relazione
* gephi_link_w_out2 contiene tutte le relazioni tra nodi tramite retweet con peso pari al numero di volte in cui si verifica la relazione, filtrato per i soli nodi con un minimo grado out pari a 2
* export_community.csv e' il file esportato da gephi con i cluster associati ad ogni nodo dall'algoritmo di clusterizzazione applicato tramite gephi

