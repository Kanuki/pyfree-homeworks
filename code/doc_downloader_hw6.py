"""
Помощник-скачиватель: открывает настоящее окно браузера (Chromium), ищет
в нём документ по запросу и скачивает файл, чьё имя совпадает с запросом
(точно или частично).

Перед первым запуском один раз выполнить:
    pip install playwright
    playwright install chromium

Использование:
    python doc_downloader_hw6.py "отчет по продажам"
    python doc_downloader_hw6.py "отчет по продажам" --exact --dir ./загрузки
    python doc_downloader_hw6.py "отчет по продажам" --headless  # без окна, в фоне
"""
import argparse
import os
import re
import time
from urllib.parse import unquote, urljoin, urlparse, parse_qs

from playwright.sync_api import sync_playwright

SEARCH_URL = 'https://duckduckgo.com/html/?q={query}'

DOC_EXTENSIONS = {
    'pdf', 'doc', 'docx', 'ppt', 'pptx', 'xls', 'xlsx',
    'txt', 'rtf', 'odt', 'ods', 'odp', 'csv', 'zip', 'rar',
}

# Обычный поиск чаще всего отдаёт ссылки на веб-страницы, а не на сами
# файлы. Если так, пробуем те же слова с оператором filetype: — это
# заставляет поисковик показывать прямые ссылки на документы.
FALLBACK_FILETYPES = ['pdf', 'doc', 'docx', 'xlsx', 'pptx']


def unwrap_ddg_link(href):
    """DuckDuckGo оборачивает ссылки в свой редирект — достаём настоящий URL."""
    parsed = urlparse(href)
    if parsed.netloc.endswith('duckduckgo.com') and parsed.path == '/l/':
        real = parse_qs(parsed.query).get('uddg')
        if real:
            return unquote(real[0])
    return href


def filename_from_url(url):
    path = urlparse(url).path
    return unquote(os.path.basename(path))


def is_document_link(url):
    ext = filename_from_url(url).rsplit('.', 1)[-1].lower()
    return '.' in filename_from_url(url) and ext in DOC_EXTENSIONS


def search_document_links(page, query, filetype=None):
    search_terms = f'{query} filetype:{filetype}' if filetype else query
    page.goto(SEARCH_URL.format(query=search_terms.replace(' ', '+')), timeout=30000)
    hrefs = page.eval_on_selector_all('a', 'els => els.map(e => e.href)')
    links = []
    for href in hrefs:
        url = unwrap_ddg_link(href)
        if is_document_link(url):
            links.append(url)
    return links


def extract_key_tokens(query):
    """Номера вида 8736-2014 (ГОСТ, СП, СНиП и т.п.) — самая надёжная примета
    файла: они почти всегда есть в имени, даже если слов вокруг них нет."""
    return [t for t in re.findall(r'\d[\d\-]*\d|\d+', query) if len(t) >= 4]


def pick_best_match(links, query, exact=False):
    query = query.strip().lower()
    for url in links:
        name = filename_from_url(url)
        stem = name.rsplit('.', 1)[0].lower()
        if stem == query:
            return url
    if exact:
        return None
    for url in links:
        if query in filename_from_url(url).lower():
            return url
    # запрос целиком не совпал — пробуем по номеру документа (для ГОСТ/СП/
    # СНиП и т.п. имя файла редко содержит слова запроса, но номер обычно есть)
    for token in extract_key_tokens(query):
        for url in links:
            if token in filename_from_url(url).lower():
                return url
    return None


class DocumentNotFoundError(Exception):
    """Ничего подходящего не нашлось — сообщение уже объясняет, почему."""


def download_file(page, url, target_dir):
    os.makedirs(target_dir, exist_ok=True)
    filename = filename_from_url(url)
    target_path = os.path.join(target_dir, filename)
    response = page.context.request.get(url)
    if not response.ok:
        raise RuntimeError(f'Не удалось скачать {url}: HTTP {response.status}')
    with open(target_path, 'wb') as f:
        f.write(response.body())
    return target_path


def find_and_download(query, target_dir='downloads', exact=False, headless=False):
    with sync_playwright() as p:
        browser = p.chromium.launch(headless=headless, slow_mo=250 if not headless else 0)
        page = browser.new_page()
        try:
            match = None
            all_links = []
            for filetype in [None, *FALLBACK_FILETYPES]:
                links = search_document_links(page, query, filetype=filetype)
                all_links.extend(links)
                match = pick_best_match(links, query, exact=exact)
                if match:
                    break
            if match is None:
                unique_count = len(set(all_links))
                if unique_count:
                    message = (f'Документ по запросу «{query}» не найден '
                               f'(проверено ссылок на документы: {unique_count}, '
                               f'но ни одно имя файла не совпало с запросом).')
                else:
                    message = (f'Документ по запросу «{query}» не найден '
                               f'(поиск не дал ни одной ссылки на документ — '
                               f'проверьте формулировку запроса или доступность поисковика).')
                raise DocumentNotFoundError(message)
            path = download_file(page, match, target_dir)
            if not headless:
                time.sleep(2)
            return path
        finally:
            browser.close()


def parse_args():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('query', help='название документа или часть названия')
    parser.add_argument('--exact', action='store_true',
                         help='искать только точное совпадение имени файла')
    parser.add_argument('--dir', default='downloads',
                         help='папка для сохранения (по умолчанию ./downloads)')
    parser.add_argument('--headless', action='store_true',
                         help='не показывать окно браузера, работать в фоне')
    return parser.parse_args()


if __name__ == '__main__':
    args = parse_args()
    try:
        path = find_and_download(args.query, target_dir=args.dir, exact=args.exact,
                                  headless=args.headless)
    except DocumentNotFoundError as e:
        print(e)
    else:
        print(f'Скачано: {path}')
