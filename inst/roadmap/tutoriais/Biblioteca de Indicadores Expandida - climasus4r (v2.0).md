# Biblioteca de Indicadores Expandida - climasus4r (v2.0)

Este documento detalha a expansão da biblioteca de indicadores, integrando métricas de saneamento (Censo 2022), o Índice de Vulnerabilidade Social (IVS), o Índice de Performance da Saúde (IPS) e indicadores de mercado de trabalho.

## 1. Novas Categorias e Indicadores

### A. Saneamento Básico (Censo 2022)
Focado na cobertura de serviços essenciais por setor censitário.

| Indicador | Sigla | Fórmula | Fonte |
| :--- | :--- | :--- | :--- |
| **Conexão de Água** | ICA | (v111 / v003) * 100 | Censo 2022 |
| **Conexão de Esgoto** | ICE | (v309 / v003) * 100 | Censo 2022 |

### B. Índice de Vulnerabilidade Social (IVS)
Índice sintético (0 a 1) composto por 16 indicadores em 3 dimensões.

**1. Infraestrutura Urbana (IVS-IU)**
- Abastecimento de água e esgoto inadequados.
- Coleta de lixo inadequada.
- Tempo de deslocamento casa-trabalho (>1h para baixa renda).

**2. Capital Humano (IVS-CH)**
- Mortalidade infantil.
- Crianças fora da escola (0-5 e 6-14 anos).
- Mães jovens (10-17 anos).
- Mães chefes de família com baixa escolaridade.
- Taxa de analfabetismo (15+ anos).
- Baixa escolaridade domiciliar.
- Jovens "nem-nem" de baixa renda (15-24 anos).

**3. Renda e Trabalho (IVS-RT)**
- Proporção de pobreza/extrema pobreza.
- Taxa de desocupação (18+ anos).
- Ocupação informal (sem fundamental completo).
- Dependência de idosos em domicílios de baixa renda.
- Taxa de trabalho infantil (10-14 anos).

### C. Índice de Performance da Saúde (IPS)
Mapeamento de desempenho municipal (0 a 1) baseado em 4 dimensões.

- **D1: Saúde Geral/APS**: Taxa de ICSAP.
- **D2: Materno-Infantil**: Gestantes adolescentes, Cobertura vacinal (<1 ano), Pré-natal precoce.
- **D3: Condições Crônicas**: Hipertensos/Diabéticos cadastrados, Internações por DM/HAS.
- **D4: Gestão**: Existência de órgão municipal de trânsito.

### D. Mercado de Trabalho e Desigualdade (IBGE)
- **Taxa de Desocupação**: % desocupados na força de trabalho.
- **Nível de Ocupação**: % ocupados na população em idade ativa (14+).
- **Taxa de Subutilização**: Composta por subocupados e força potencial.
- **Índice de Gini**: Medida de desigualdade de renda.

## 2. Métodos de Cálculo Sugeridos

### Normalização (Min-Max)
Para índices sintéticos (IVS, IPS), os indicadores devem ser normalizados:
- **Polaridade Positiva** (Maior é melhor): `(Valor - Min) / (Max - Min)`
- **Polaridade Negativa** (Menor é melhor): `(Max - Valor) / (Max - Min)`

### Agregação
- **IVS**: Média aritmética simples das 3 dimensões.
- **IPS**: Média ponderada: `0.1*D1 + 0.3*D2 + 0.5*D3 + 0.1*D4`.
