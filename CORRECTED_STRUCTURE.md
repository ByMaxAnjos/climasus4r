# Estrutura Corrigida do Site Multilíngue ClimaSus4R

## 🎯 Problema Identificado

O `pkgdown::build_site()` gera automaticamente um `index.html` a partir do `README.md`, o que estava sobrescrevendo nossa página de seleção de idiomas.

## ✅ Solução Implementada

A estrutura foi corrigida para separar completamente:
- **Página de seleção de idiomas** (raiz do site)
- **Sites completos em cada idioma** (subpastas `/pt/`, `/en/`, `/es/`)

## 📁 Estrutura Final do Site

```
https://bymaxanjos.github.io/climasus4r/
│
├── index.html                      # Página de seleção de idiomas (standalone)
│
├── pt/                             # Site completo em Português
│   ├── index.html                  # Gerado do README-pt.md
│   ├── articles/
│   │   ├── about.html
│   │   └── tutorials.html
│   ├── reference/
│   └── pkgdown/
│       └── assets/
│           └── custom.css
│
├── en/                             # Site completo em Inglês
│   ├── index.html                  # Gerado do README-en.md
│   ├── articles/
│   │   ├── about.html
│   │   └── tutorials.html
│   ├── reference/
│   └── pkgdown/
│       └── assets/
│           └── custom.css
│
└── es/                             # Site completo em Espanhol
    ├── index.html                  # Gerado do README-es.md
    ├── articles/
    │   ├── about.html
    │   └── tutorials.html
    ├── reference/
    └── pkgdown/
        └── assets/
            └── custom.css
```

## 🔧 Arquivos Criados/Modificados

### 1. READMEs Específicos por Idioma

- **README.md** - Versão original do pacote (mantida para desenvolvimento)
- **README-pt.md** - Versão portuguesa para o site `/pt/`
- **README-en.md** - Versão inglesa para o site `/en/`
- **README-es.md** - Versão espanhola para o site `/es/`

Cada README específico inclui:
- Conteúdo traduzido
- Links para os outros idiomas
- Exemplos de código no idioma apropriado

### 2. Workflow do GitHub Actions Corrigido

O workflow agora:

1. **Para cada idioma (PT, EN, ES):**
   - Copia o README específico (`README-pt.md` → `README.md`)
   - Copia as vignettes do idioma
   - Copia a configuração pkgdown do idioma
   - Executa `pkgdown::build_site()`
   - Move o resultado para `deploy/{idioma}/`
   - Limpa os arquivos temporários

2. **Após construir todos os idiomas:**
   - Copia o `index.html` standalone para `deploy/`
   - Copia os assets CSS para cada subpasta de idioma

3. **Deploy:**
   - Publica todo o conteúdo de `deploy/` para o GitHub Pages

## 🎨 Página de Seleção de Idiomas

O `index.html` na raiz é um arquivo HTML standalone que:
- Não é gerado pelo pkgdown
- Contém os 4 cards elegantes (PT, EN, ES, AI Assistant)
- Tem design moderno com gradiente e animações
- Redireciona para `/pt/`, `/en/`, ou `/es/`

## 🚀 Como Funciona o Fluxo

### Durante o Build (GitHub Actions)

```
1. Checkout do código
2. Instalar R e dependências
3. Para PT:
   - README-pt.md → README.md
   - vignettes-pt/ → vignettes/
   - _pkgdown-pt.yml → _pkgdown.yml
   - pkgdown::build_site()
   - docs/ → deploy/pt/
4. Para EN:
   - README-en.md → README.md
   - vignettes-en/ → vignettes/
   - _pkgdown-en.yml → _pkgdown.yml
   - pkgdown::build_site()
   - docs/ → deploy/en/
5. Para ES:
   - README-es.md → README.md
   - vignettes-es/ → vignettes/
   - _pkgdown-es.yml → _pkgdown.yml
   - pkgdown::build_site()
   - docs/ → deploy/es/
6. Copiar index.html → deploy/
7. Deploy de deploy/ → gh-pages branch
```

### Navegação do Usuário

```
1. Usuário acessa: https://bymaxanjos.github.io/climasus4r/
   → Vê a página com 4 cards

2. Clica no card "Português"
   → Redireciona para: /pt/
   → Vê o site completo em português

3. No site PT, pode navegar:
   - Tutoriais: /pt/articles/tutorials.html
   - Sobre: /pt/articles/about.html
   - Referências: /pt/reference/index.html
   - Trocar idioma: Links no topo levam para /en/ ou /es/
```

## ✨ Vantagens Desta Estrutura

1. **Separação Clara**: Landing page separada dos sites de idioma
2. **Sem Conflitos**: O pkgdown não sobrescreve o index.html principal
3. **Manutenção Simples**: Cada idioma tem seu próprio README e vignettes
4. **Navegação Intuitiva**: Links entre idiomas funcionam corretamente
5. **Escalável**: Fácil adicionar novos idiomas no futuro

## 📝 Para Adicionar Conteúdo

### Adicionar um Novo Tutorial

1. Crie o arquivo em cada pasta de vignettes:
   - `vignettes-pt/novo-tutorial.Rmd`
   - `vignettes-en/new-tutorial.Rmd`
   - `vignettes-es/nuevo-tutorial.Rmd`

2. Atualize a página hub de tutoriais em cada idioma

3. Faça commit e push → GitHub Actions reconstrói automaticamente

### Atualizar o Design

1. Edite `pkgdown/assets/custom.css`
2. As mudanças serão aplicadas a todos os idiomas automaticamente

### Modificar a Página de Seleção

1. Edite `index.html` diretamente
2. Faça commit e push

## 🔍 Verificação

Para verificar se tudo está funcionando:

1. Acesse: `https://bymaxanjos.github.io/climasus4r/`
   - Deve mostrar os 4 cards

2. Clique em cada card de idioma
   - Deve levar para o site completo naquele idioma

3. Em cada site de idioma, verifique:
   - Navegação funciona
   - Links entre idiomas funcionam
   - CSS customizado está aplicado
   - Conteúdo está no idioma correto

## 🎉 Status

✅ Estrutura corrigida
✅ READMEs específicos criados
✅ Workflow atualizado
✅ Landing page preservada
✅ Pronto para deploy!
