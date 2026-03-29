/**
 * climasus4r Landing Page
 * Neo-Brutalism Soft with GSAP Animations
 */

document.addEventListener('DOMContentLoaded', () => {
    // Initialize all modules
    initParticles();
    initAnimations();
    initNavigation();
    initLanguageSwitcher();
    initSmoothScroll();
    initInteractiveElements();
});

/* ========================================
   Particles System
   ======================================== */
function initParticles() {
    const container = document.getElementById('particles-container');
    if (!container) return;

    const canvas = document.createElement('canvas');
    container.appendChild(canvas);
    const ctx = canvas.getContext('2d');

    let particles = [];
    const particleCount = 50;
    const connectionDistance = 150;

    // Resize canvas
    function resizeCanvas() {
        canvas.width = window.innerWidth;
        canvas.height = window.innerHeight;
    }
    resizeCanvas();
    window.addEventListener('resize', resizeCanvas);

    // Particle class
    class Particle {
        constructor() {
            this.x = Math.random() * canvas.width;
            this.y = Math.random() * canvas.height;
            this.vx = (Math.random() - 0.5) * 0.5;
            this.vy = (Math.random() - 0.5) * 0.5;
            this.radius = Math.random() * 3 + 1;
            this.color = Math.random() > 0.5 ? 'rgba(0, 255, 136, 0.6)' : 'rgba(168, 85, 247, 0.6)';
        }

        update() {
            this.x += this.vx;
            this.y += this.vy;

            // Boundary check
            if (this.x < 0 || this.x > canvas.width) this.vx *= -1;
            if (this.y < 0 || this.y > canvas.height) this.vy *= -1;
        }

        draw() {
            ctx.beginPath();
            ctx.arc(this.x, this.y, this.radius, 0, Math.PI * 2);
            ctx.fillStyle = this.color;
            ctx.fill();
        }
    }

    // Initialize particles
    for (let i = 0; i < particleCount; i++) {
        particles.push(new Particle());
    }

    // Draw connections
    function drawConnections() {
        for (let i = 0; i < particles.length; i++) {
            for (let j = i + 1; j < particles.length; j++) {
                const dx = particles[i].x - particles[j].x;
                const dy = particles[i].y - particles[j].y;
                const distance = Math.sqrt(dx * dx + dy * dy);

                if (distance < connectionDistance) {
                    const opacity = 1 - (distance / connectionDistance);
                    ctx.beginPath();
                    ctx.moveTo(particles[i].x, particles[i].y);
                    ctx.lineTo(particles[j].x, particles[j].y);
                    ctx.strokeStyle = `rgba(0, 255, 136, ${opacity * 0.3})`;
                    ctx.lineWidth = 1;
                    ctx.stroke();
                }
            }
        }
    }

    // Animation loop
    function animate() {
        ctx.clearRect(0, 0, canvas.width, canvas.height);

        particles.forEach(particle => {
            particle.update();
            particle.draw();
        });

        drawConnections();
        requestAnimationFrame(animate);
    }

    animate();

    // Pause animation when tab is not visible
    document.addEventListener('visibilitychange', () => {
        if (document.hidden) {
            ctx.clearRect(0, 0, canvas.width, canvas.height);
        } else {
            animate();
        }
    });
}

/* ========================================
   GSAP Animations
   ======================================== */
function initAnimations() {
    // Check if GSAP is available
    if (typeof gsap === 'undefined') {
        console.warn('GSAP not loaded, using CSS animations');
        initCSSAnimations();
        return;
    }

    // Register ScrollTrigger
    gsap.registerPlugin(ScrollTrigger);

    // Hero animations
    const heroItems = document.querySelectorAll('.hero .animate-item');
    gsap.to(heroItems, {
        opacity: 1,
        y: 0,
        duration: 0.8,
        stagger: 0.15,
        ease: 'power3.out',
        delay: 0.3
    });

    // Section headers
    const sectionHeaders = document.querySelectorAll('.animate-section');
    sectionHeaders.forEach(header => {
        gsap.to(header, {
            scrollTrigger: {
                trigger: header,
                start: 'top 85%',
                toggleActions: 'play none none reverse'
            },
            opacity: 1,
            y: 0,
            duration: 0.8,
            ease: 'power3.out'
        });
    });

    // Bento cards with stagger
    const bentoCards = document.querySelectorAll('.animate-card');
    bentoCards.forEach((card, index) => {
        const delay = parseInt(card.dataset.delay) || 0;

        gsap.to(card, {
            scrollTrigger: {
                trigger: card,
                start: 'top 90%',
                toggleActions: 'play none none reverse'
            },
            opacity: 1,
            y: 0,
            duration: 0.6,
            delay: delay * 0.1,
            ease: 'power2.out'
        });
    });

    // Stats counter animation
    const statNumbers = document.querySelectorAll('.stat-number');
    statNumbers.forEach(stat => {
        const finalValue = stat.textContent;
        const numericValue = parseInt(finalValue.replace(/\D/g, ''));

        gsap.from(stat, {
            scrollTrigger: {
                trigger: stat,
                start: 'top 85%',
                toggleActions: 'play none none reverse'
            },
            textContent: 0,
            duration: 2,
            ease: 'power2.out',
            snap: { textContent: 1 },
            onUpdate: function() {
                const current = Math.round(gsap.getProperty(stat, 'textContent'));
                if (finalValue.includes('+')) {
                    stat.textContent = current + '+';
                } else if (finalValue.includes('.')) {
                    stat.textContent = current.toLocaleString();
                } else {
                    stat.textContent = current;
                }
            }
        });
    });

    // Parallax effect for hero
    gsap.to('.hero::before', {
        scrollTrigger: {
            trigger: '.hero',
            start: 'top top',
            end: 'bottom top',
            scrub: 1
        },
        y: 200,
        ease: 'none'
    });

    // Feature cards hover effect
    const featureCards = document.querySelectorAll('.feature-card');
    featureCards.forEach(card => {
        card.addEventListener('mouseenter', () => {
            gsap.to(card.querySelector('.feature-icon'), {
                scale: 1.1,
                rotation: 5,
                duration: 0.3,
                ease: 'power2.out'
            });
        });
        card.addEventListener('mouseleave', () => {
            gsap.to(card.querySelector('.feature-icon'), {
                scale: 1,
                rotation: 0,
                duration: 0.3,
                ease: 'power2.out'
            });
        });
    });

    // Navbar background on scroll
    ScrollTrigger.create({
        start: 'top -100',
        end: 99999,
        toggleClass: {
            className: 'scrolled',
            targets: '.navbar'
        }
    });
}

/* ========================================
   CSS Fallback Animations
   ======================================== */
function initCSSAnimations() {
    // Fallback for browsers without GSAP
    const observerOptions = {
        threshold: 0.1,
        rootMargin: '0px 0px -50px 0px'
    };

    const observer = new IntersectionObserver((entries) => {
        entries.forEach(entry => {
            if (entry.isIntersecting) {
                entry.target.classList.add('visible');
                observer.unobserve(entry.target);
            }
        });
    }, observerOptions);

    // Observe all animated elements
    document.querySelectorAll('.animate-item, .animate-section, .animate-card').forEach(el => {
        observer.observe(el);
    });
}

/* ========================================
   Navigation
   ======================================== */
function initNavigation() {
    const navbar = document.getElementById('navbar');
    const hamburger = document.getElementById('hamburger');
    const navLinks = document.getElementById('nav-links');

    // Mobile menu toggle
    if (hamburger) {
        hamburger.addEventListener('click', () => {
            hamburger.classList.toggle('active');
            navLinks.classList.toggle('active');

            // Animate hamburger
            const spans = hamburger.querySelectorAll('span');
            if (hamburger.classList.contains('active')) {
                gsap.to(spans[0], { rotation: 45, y: 7, duration: 0.3 });
                gsap.to(spans[1], { opacity: 0, duration: 0.3 });
                gsap.to(spans[2], { rotation: -45, y: -7, duration: 0.3 });
            } else {
                gsap.to(spans, { rotation: 0, y: 0, opacity: 1, duration: 0.3 });
            }
        });
    }

    // Close mobile menu on link click
    navLinks.querySelectorAll('a').forEach(link => {
        link.addEventListener('click', () => {
            hamburger?.classList.remove('active');
            navLinks.classList.remove('active');
        });
    });

    // Navbar scroll effect
    let lastScroll = 0;
    window.addEventListener('scroll', () => {
        const currentScroll = window.pageYOffset;

        if (currentScroll > 100) {
            navbar.style.transform = 'translateY(-100%)';
        } else {
            navbar.style.transform = 'translateY(0)';
        }

        lastScroll = currentScroll;
    });
}

/* ========================================
   Language Switcher
   ======================================== */
function initLanguageSwitcher() {
    const langBtns = document.querySelectorAll('.lang-btn');
    let currentLang = 'pt';

    // Translations
    const translations = {
        pt: {
            'hero.badge': 'Versão 0.0.9000 Disponível',
            'hero.title.line1': 'Ciência Climática',
            'hero.title.line2': 'Integrada ao SUS',
            'hero.subtitle': 'Um pacote em R para obtenção de dados, diagnóstico, previsão e análise espaço-temporal de dados climáticos, ambientais e de saúde, com foco na saúde pública brasileira.',
            'hero.cta': 'Começar Agora',
            'stats.functionalities': 'Funcionalidades',
            'stats.diseases': 'Grupos de Doenças',
            'stats.systems': 'Sistemas SUS',
            'stats.languages': 'Idiomas',
            'about.label': 'Sobre o Pacote',
            'about.title': 'O que é o climasus4r?',
            'about.text': 'climasus4r é uma plataforma científica em R para obtenção, integração, padronização e análise de dados de saúde, clima e ambiente no Brasil, com foco na previsão bioclimática. Desenvolvido no âmbito do INCT Conexão e do Centro de Clima e Saúde de Rondônia (CCSRO–Fiocruz), o pacote automatiza todo o fluxo de trabalho epidemiológico — desde a aquisição de dados do DATASUS até a geração de séries temporais prontas para modelagem climática e vigilância em saúde.',
            'features.label': 'Funcionalidades',
            'features.title': 'Cinco fases integradas',
            'features.subtitle': 'Do dado bruto à inteligência para políticas públicas em saúde.',
            'features.f1.title': 'Fase 1 — Infraestrutura de Dados',
            'features.f1.desc': 'Aquisição paralela, limpeza, padronização multilíngue, filtragem por CID-10, criação de variáveis epidemiológicas, agregação temporal flexível, relatórios de qualidade e exportação com metadados prontos para análise científica.',
            'features.f2.title': 'Fase 2 — Integração Socioeconômica',
            'features.f2.desc': 'Vinculação espacial de limites geográficos, integração de dados do IBGE (população, PIB, IDH), correspondência de setores censitários e operações espaciais ponderadas pela população para análise de desigualdades em saúde.',
            'features.f3.title': 'Fase 3 — Integração Climática',
            'features.f3.desc': 'Importação de dados meteorológicos (INMET, FIORES, INCT-CONEXÃO), qualidade do ar (INPE, CETESB), dados de satélite (MODIS, Sentinel) e algoritmos de correspondência de exposição climática.',
            'features.f4.title': 'Fase 4 — Análise Espacial Avançada',
            'features.f4.desc': 'Modelos espaciais bayesianos, detecção de clusters (SaTScan, Kulldorff), indicadores locais de autocorrelação (LISA), regressões espaciais e modelos de risco relativo para internações e mortalidade associados ao clima.',
            'features.f5.title': 'Fase 5 — Modelagem Temporal e Preditiva',
            'features.f5.desc': 'Modelos de defasagem distribuída não linear (DLNM), fração atribuível, decomposição de séries temporais, aprendizado de máquina e bioprognose climática baseada em previsões meteorológicas e cenários climáticos.',
            'langs.label': 'Acesso Multilíngue',
            'langs.title': 'Documentação em 3 idiomas + IA',
            'langs.subtitle': 'Acesse a documentação completa no seu idioma ou use o assistente de IA para tirar dúvidas instantaneamente.',
            'langs.pt.desc': 'Documentação completa, tutoriais e referências em português brasileiro.',
            'langs.en.desc': 'Full documentation, tutorials and function reference in English.',
            'langs.es.desc': 'Documentación completa, tutoriales y referencia de funciones en español.',
            'langs.ai.desc': 'Converse com um especialista em IA treinado no climasus4r. Tire dúvidas, obtenha exemplos e explore o pacote.',
            'team.label': 'Quem Somos',
            'team.title': 'Nossa Equipe',
            'team.subtitle': 'Desenvolvido por especialistas dedicados à interseção entre clima e saúde pública.',
            'team.role.mentor': 'Mentor e Desenvolvedor R',
            'team.role.contributor': 'Contribuidor R',
            'team.role.consultant': 'Consultor Científico',
            'funding.label': 'Apoio Institucional',
            'funding.title': 'Apoio e Financiamento',
            'funding.text': 'O projeto climasus4r é financiado pelo Ministério da Saúde e pela Fundação Oswaldo Cruz Rondônia (Fiocruz-RO), por meio do Centro de Clima e Saúde de Rondônia (CCSRO), no âmbito do INCT-CONEXAO, processo CNPq nº 408474/2024-6.',
            'cta_final.title': 'Comece a usar o climasus4r hoje',
            'cta_final.subtitle': 'Transforme dados climáticos e de saúde em inteligência para políticas públicas.'
        },
        en: {
            'hero.badge': 'Version 0.0.9000 Available',
            'hero.title.line1': 'Climate Science',
            'hero.title.line2': 'Integrated with SUS',
            'hero.subtitle': 'An R package for obtaining data, diagnosis, forecasting, and spatio-temporal analysis of climate, environmental, and health data, focusing on Brazilian public health.',
            'hero.cta': 'Get Started',
            'stats.functionalities': 'Funcionalities',
            'stats.diseases': 'Disease Groups',
            'stats.systems': 'SUS Systems',
            'stats.languages': 'Languages',
            'about.label': 'About the Package',
            'about.title': 'What is climasus4r?',
            'about.text': 'climasus4r is a scientific platform in R for obtaining, integrating, standardizing, and analyzing health, climate, and environmental data in Brazil, with a focus on bioclimatic forecasting. Developed within the scope of INCT Conexão and the Center for Climate and Health of Rondônia (CCSRO–Fiocruz), the package automates the entire epidemiological workflow — from data acquisition from DATASUS to the generation of time series ready for climate modeling and health surveillance.',
            'features.label': 'Features',
            'features.title': 'Five Integrated Phases',
            'features.subtitle': 'From raw data to intelligence for public health policies.',
            'features.f1.title': 'Phase 1 — Data Infrastructure',
            'features.f1.desc': 'Parallel acquisition, cleaning, multilingual standardization, filtering by ICD-10, creation of epidemiological variables, flexible temporal aggregation, quality reports, and export with metadata ready for scientific analysis.',
            'features.f2.title': 'Phase 2 — Socioeconomic Integration',
            'features.f2.desc': 'Spatial linking of geographical boundaries, integration of IBGE data (population, GDP, HDI), matching of census tracts, and population-weighted spatial operations for analyzing health inequalities.',
            'features.f3.title': 'Phase 3 — Climate Integration',
            'features.f3.desc': 'Import of meteorological data (INMET, FIORES, INCT-CONEXÃO), air quality (INPE, CETESB), satellite data (MODIS, Sentinel), and climate exposure matching algorithms.',
            'features.f4.title': 'Phase 4 — Advanced Spatial Analysis',
            'features.f4.desc': 'Bayesian spatial models, cluster detection (SaTScan, Kulldorff), local indicators of spatial autocorrelation (LISA), spatial regressions, and relative risk models for hospitalizations and mortality associated with climate.',
            'features.f5.title': 'Phase 5 — Temporal and Predictive Modeling',
            'features.f5.desc': 'Distributed lag non-linear models (DLNM), attributable fraction, time series decomposition, machine learning, and bioclimatic prognosis based on weather forecasts and climate scenarios.',
            'langs.label': 'Multilingual Access',
            'langs.title': 'Documentation in 3 Languages + AI',
            'langs.subtitle': 'Access the full documentation in your language or use the AI assistant to get instant answers.',
            'langs.pt.desc': 'Complete documentation, tutorials, and references in Brazilian Portuguese.',
            'langs.en.desc': 'Full documentation, tutorials and function reference in English.',
            'langs.es.desc': 'Complete documentation, tutorials, and function reference in Spanish.',
            'langs.ai.desc': 'Chat with an AI specialist trained on the climasus4r package. Ask questions, get examples, and explore the package.',
            'team.label': 'Who We Are',
            'team.title': 'Our Team',
            'team.subtitle': 'Developed by experts dedicated to the intersection of climate and public health.',
            'team.role.mentor': 'Mentor and R Developer',
            'team.role.consultant': 'Scientific Consultant',
            'funding.label': 'Institutional Support',
            'funding.title': 'Support and Funding',
            'funding.text': 'The climasus4r project is funded by the Ministry of Health and the Oswaldo Cruz Foundation Rondônia (Fiocruz-RO), through the Center for Climate and Health of Rondônia (CCSRO), within the scope of INCT-CONEXAO, CNPq process No. 408474/2024-6.',
            'cta_final.title': 'Start using climasus4r today',
            'cta_final.subtitle': 'Transform climate and health data into intelligence for public policies.'
        },
        es: {
            'hero.badge': 'Versión 0.0.9000 Disponible',
            'hero.title.line1': 'Ciencia Climática',
            'hero.title.line2': 'Integrada al SUS',
            'hero.subtitle': 'Un paquete en R para la obtención de datos, diagnóstico, pronóstico y análisis espacio-temporal de datos climáticos, ambientales y de salud, con foco en la salud pública brasileña.',
            'hero.cta': 'Empezar',
            'stats.functionalities': 'Funcionalidades',
            'stats.diseases': 'Grupos de Enfermedades',
            'stats.systems': 'Sistemas SUS',
            'stats.languages': 'Idiomas',
            'about.label': 'Sobre el Paquete',
            'about.title': '¿Qué es climasus4r?',
            'about.text': 'climasus4r es una plataforma científica en R para la obtención, integración, estandarización y análisis de datos de salud, clima y ambiente en Brasil, con foco en el pronóstico bioclimático. Desarrollado en el ámbito del INCT Conexão y del Centro de Clima y Salud de Rondônia (CCSRO–Fiocruz), el paquete automatiza todo el flujo de trabajo epidemiológico — desde la adquisición de datos del DATASUS hasta la generación de series temporales listas para el modelado climático y la vigilancia en salud.',
            'features.label': 'Funcionalidades',
            'features.title': 'Cinco fases integradas',
            'features.subtitle': 'Del dato bruto a la inteligencia para políticas públicas en salud.',
            'features.f1.title': 'Fase 1 — Infraestructura de Datos',
            'features.f1.desc': 'Adquisición paralela, limpieza, estandarización multilingüe, filtrado por CIE-10, creación de variables epidemiológicas, agregación temporal flexible, informes de calidad y exportación con metadatos listos para el análisis científico.',
            'features.f2.title': 'Fase 2 — Integración Socioeconómica',
            'features.f2.desc': 'Vinculación espacial de límites geográficos, integración de datos del IBGE (población, PIB, IDH), correspondencia de sectores censales y operaciones espaciales ponderadas por la población para el análisis de desigualdades en salud.',
            'features.f3.title': 'Fase 3 — Integración Climática',
            'features.f3.desc': 'Importación de datos meteorológicos (INMET, FIORES, INCT-CONEXÃO), calidad del aire (INPE, CETESB), datos de satélite (MODIS, Sentinel) y algoritmos de correspondencia de exposición climática.',
            'features.f4.title': 'Fase 4 — Análisis Espacial Avanzado',
            'features.f4.desc': 'Modelos espaciales bayesianos, detección de clusters (SaTScan, Kulldorff), indicadores locales de autocorrelación espacial (LISA), regresiones espaciales y modelos de riesgo relativo para hospitalizaciones y mortalidad asociados al clima.',
            'features.f5.title': 'Fase 5 — Modelado Temporal y Predictivo',
            'features.f5.desc': 'Modelos de rezago distribuido no lineal (DLNM), fracción atribuible, descomposición de series temporales, aprendizaje automático y biopronóstico climático basado en pronósticos meteorológicos y escenarios climáticos.',
            'langs.label': 'Acceso Multilingüe',
            'langs.title': 'Documentación en 3 idiomas + IA',
            'langs.subtitle': 'Acceda a la documentación completa en su idioma o use el asistente de IA para obtener respuestas instantáneas.',
            'langs.pt.desc': 'Documentación completa, tutoriales y referencias en portugués brasileño.',
            'langs.en.desc': 'Complete documentation, tutorials and function reference in English.',
            'langs.es.desc': 'Documentación completa, tutoriales y referencia de funciones en español.',
            'langs.ai.desc': 'Hable con un especialista en IA entrenado en el paquete climasus4r. Haga preguntas, obtenga ejemplos y explore el paquete.',
            'team.label': 'Quiénes Somos',
            'team.title': 'Nuestro Equipo',
            'team.subtitle': 'Desarrollado por expertos dedicados a la intersección entre clima y salud pública.',
            'team.role.mentor': 'Mentor y Desarrollador R',
            'team.role.contributor': 'Contribuidor R',
            'team.role.consultant': 'Consultor Científico',
            'funding.label': 'Apoyo Institucional',
            'funding.title': 'Apoyo y Financiamiento',
            'funding.text': 'El proyecto climasus4r es financiado por el Ministerio de Salud y la Fundación Oswaldo Cruz Rondônia (Fiocruz-RO), a través del Centro de Clima y Salud de Rondônia (CCSRO), en el ámbito del INCT-CONEXAO, proceso CNPq nº 408474/2024-6.',
            'cta_final.title': 'Comience a usar climasus4r hoy',
            'cta_final.subtitle': 'Transforme datos climáticos y de salud en inteligencia para políticas públicas.'
        }
    };

    // CTA links per language
    const ctaLinks = {
        pt: 'https://bymaxanjos.github.io/climasus4r/pt/index.html',
        en: 'https://bymaxanjos.github.io/climasus4r/en/index.html',
        es: 'https://bymaxanjos.github.io/climasus4r/es/index.html'
    };

    // Update translations
    function updateLanguage(lang) {
        currentLang = lang;

        // Update active button
        langBtns.forEach(btn => {
            btn.classList.toggle('active', btn.dataset.lang === lang);
        });

        // Update all translated elements
        document.querySelectorAll('[data-translate]').forEach(el => {
            const key = el.dataset.translate;
            const value = translations[lang]?.[key];
            if (value) {
                el.textContent = value;
            }
        });

        // Update CTA links
        document.querySelectorAll('a[href*="bymaxanjos.github.io/climasus4r"]').forEach(link => {
            link.href = ctaLinks[lang];
        });

        // Update HTML lang attribute
        document.documentElement.lang = lang === 'pt' ? 'pt-BR' : lang;

        // Save preference
        localStorage.setItem('climasus4r-lang', lang);
    }

    // Initialize from localStorage
    const savedLang = localStorage.getItem('climasus4r-lang');
    if (savedLang && translations[savedLang]) {
        updateLanguage(savedLang);
    }

    // Add click listeners
    langBtns.forEach(btn => {
        btn.addEventListener('click', () => {
            const lang = btn.dataset.lang;
            updateLanguage(lang);

            // Animate button
            if (typeof gsap !== 'undefined') {
                gsap.from(btn, {
                    scale: 0.9,
                    duration: 0.2,
                    ease: 'back.out(1.7)'
                });
            }
        });
    });
}

/* ========================================
   Smooth Scroll
   ======================================== */
function initSmoothScroll() {
    document.querySelectorAll('a[href^="#"]').forEach(anchor => {
        anchor.addEventListener('click', function(e) {
            const href = this.getAttribute('href');
            if (href === '#') return;

            e.preventDefault();
            const target = document.querySelector(href);

            if (target) {
                const offset = 80;
                const targetPosition = target.getBoundingClientRect().top + window.pageYOffset - offset;

                window.scrollTo({
                    top: targetPosition,
                    behavior: 'smooth'
                });

                // Update URL
                history.pushState(null, null, href);
            }
        });
    });
}

/* ========================================
   Interactive Elements
   ======================================== */
function initInteractiveElements() {
    // Button ripple effect
    document.querySelectorAll('.btn-primary, .btn-nav-primary').forEach(btn => {
        btn.addEventListener('click', function(e) {
            const rect = this.getBoundingClientRect();
            const x = e.clientX - rect.left;
            const y = e.clientY - rect.top;

            const ripple = document.createElement('span');
            ripple.style.cssText = `
                position: absolute;
                background: rgba(255, 255, 255, 0.5);
                border-radius: 50%;
                transform: scale(0);
                animation: ripple 0.6s linear;
                pointer-events: none;
                left: ${x}px;
                top: ${y}px;
                width: 100px;
                height: 100px;
                margin-left: -50px;
                margin-top: -50px;
            `;

            this.style.position = 'relative';
            this.style.overflow = 'hidden';
            this.appendChild(ripple);

            setTimeout(() => ripple.remove(), 600);
        });
    });

    // Add ripple animation
    const style = document.createElement('style');
    style.textContent = `
        @keyframes ripple {
            to {
                transform: scale(4);
                opacity: 0;
            }
        }
    `;
    document.head.appendChild(style);

    // Card tilt effect
    document.querySelectorAll('.bento-card').forEach(card => {
        card.addEventListener('mousemove', function(e) {
            const rect = this.getBoundingClientRect();
            const x = e.clientX - rect.left;
            const y = e.clientY - rect.top;

            const centerX = rect.width / 2;
            const centerY = rect.height / 2;

            const rotateX = (y - centerY) / 20;
            const rotateY = (centerX - x) / 20;

            this.style.transform = `perspective(1000px) rotateX(${rotateX}deg) rotateY(${rotateY}deg) translateY(-6px)`;
        });

        card.addEventListener('mouseleave', function() {
            this.style.transform = '';
        });
    });

    // Feature tags hover
    document.querySelectorAll('.feature-tag-small').forEach(tag => {
        tag.addEventListener('mouseenter', function() {
            this.style.transform = 'scale(1.05)';
            this.style.background = 'var(--primary-light)';
        });
        tag.addEventListener('mouseleave', function() {
            this.style.transform = '';
            this.style.background = '';
        });
    });
}

/* ========================================
   Utility Functions
   ======================================== */

// Debounce function
function debounce(func, wait) {
    let timeout;
    return function executedFunction(...args) {
        const later = () => {
            clearTimeout(timeout);
            func(...args);
        };
        clearTimeout(timeout);
        timeout = setTimeout(later, wait);
    };
}

// Throttle function
function throttle(func, limit) {
    let inThrottle;
    return function(...args) {
        if (!inThrottle) {
            func.apply(this, args);
            inThrottle = true;
            setTimeout(() => inThrottle = false, limit);
        }
    };
}

// Performance observer for lazy loading
if ('IntersectionObserver' in window) {
    const lazyImages = document.querySelectorAll('img[data-src]');
    const imageObserver = new IntersectionObserver((entries) => {
        entries.forEach(entry => {
            if (entry.isIntersecting) {
                const img = entry.target;
                img.src = img.dataset.src;
                img.removeAttribute('data-src');
                imageObserver.unobserve(img);
            }
        });
    });

    lazyImages.forEach(img => imageObserver.observe(img));
}
