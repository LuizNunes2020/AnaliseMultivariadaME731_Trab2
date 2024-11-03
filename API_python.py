import re
import requests
import csv
from bs4 import BeautifulSoup

# Definir os headers para a requisição
headers = {
    'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.3'
}

class Vlr:
    def get_parse(self, url):
        """
        Faz uma requisição ao URL e retorna um objeto BeautifulSoup e o status code
        :param url: A URL da página que você quer parsear
        :return: Um objeto BeautifulSoup e o status code
        """
        resp = requests.get(url, headers=headers)
        html, status_code = resp.text, resp.status_code
        return BeautifulSoup(html, 'html.parser'), status_code

    def vlr_stats(self, url):
        html, status = self.get_parse(url)
        result = []
        for item in html.select("tbody tr"):
            player_data = item.text.strip().replace("\t", "").split("\n")
            player_name = player_data[0].strip()

            # Obter o nome da organização
            org = player_data[1].strip() if len(player_data) > 1 else "N/A"

            # Extrair todos os valores em cada coluna de forma sequencial
            columns = [stat.text.strip() for stat in item.select("td")]

            # Verificar se temos o número esperado de colunas
            if len(columns) >= 21:  
                rnd = columns[2]  
                rating = columns[3]
                acs = columns[4]
                kd = columns[5]
                kast = columns[6]
                adr = columns[7]
                kpr = columns[8]
                apr = columns[9]
                fkpr = columns[10]
                fdpr = columns[11]
                hs = columns[12]
                cl_perc = columns[13]
                cl = columns[14]
                kmax = columns[15]
                kills = columns[16]
                deaths = columns[17]
                assists = columns[18]
                first_kills = columns[19]
                first_deaths = columns[20]
            else:
                # Caso o número de colunas seja menor, marque como "N/A"
                rnd = rating = acs = kd = kast = adr = kpr = apr = fkpr = fdpr = hs = cl_perc = cl = kmax = kills = deaths = assists = first_kills = first_deaths = "N/A"

            result.append(
                {
                    "player": player_name,
                    "org": org,
                    "rounds_played": rnd,
                    "rating": rating,
                    "average_combat_score": acs,
                    "kill_deaths": kd,
                    "kill_assists_survived_traded": kast,
                    "average_damage_per_round": adr,
                    "kills_per_round": kpr,
                    "assists_per_round": apr,
                    "first_kills_per_round": fkpr,
                    "first_deaths_per_round": fdpr,
                    "headshot_percentage": hs,
                    "clutch_success_percentage": cl_perc,
                    "clutch_wins": cl,
                    "max_kills": kmax,
                    "kills": kills,
                    "deaths": deaths,
                    "assists": assists,
                    "first_kills": first_kills,
                    "first_deaths": first_deaths
                }
            )

        return result

    def save_to_csv(self, data, filename):
        """
        Salva os dados em um arquivo CSV
        :param data: Lista de dicionários contendo os dados
        :param filename: Nome do arquivo CSV a ser salvo
        """
        keys = data[0].keys()
        with open(filename, 'w', newline='', encoding='utf-8') as output_file:
            dict_writer = csv.DictWriter(output_file, fieldnames=keys)
            dict_writer.writeheader()
            dict_writer.writerows(data)

if __name__ == "__main__":
    url = "https://www.vlr.gg/stats"
    vlr = Vlr()
    data = vlr.vlr_stats(url)
    vlr.save_to_csv(data, 'dados_valorant_2.csv')
