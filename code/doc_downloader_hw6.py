"""
Помощник-скачиватель: ищет документ в интернете по запросу и скачивает
файл, чьё имя совпадает с запросом (точно или частично).

Запуск браузера — через Playwright/Chromium. Перед первым запуском один раз
выполнить:
    pip install playwright
    playwright install chromium

Использование:
    python doc_downloader_hw6.py "отчет по продажам"
    python doc_downloader_hw6.py "отчет по продажам" --exact --dir ./загрузки
"""
import argparse
import os
from urllib.parse import unquote, urljoin, urlparse, parse_qs

from playwright.sync_api import sync_playwright

SEARCH_URL = 'https://duckduckgo.com/html/?q={query}'

DOC_EXTENSIONS = {
    'pdf', 'doc', 'docx', 'ppt', 'pptx', 'xls', 'xlsx',
    'txt', 'rtf', 'odt', 'ods', 'odp', 'csv', 'zip', 'rar',
}


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


def search_document_links(page, query):
    page.goto(SEARCH_URL.format(query=query.replace(' ', '+')), timeout=30000)
    hrefs = page.eval_on_selector_all('a', 'els => els.map(e => e.href)')
    links = []
    for href in hrefs:
        url = unwrap_ddg_link(href)
        if is_document_link(url):
            links.append(url)
    return links


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
    return None


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


def find_and_download(query, target_dir='downloads', exact=False):
    with sync_playwright() as p:
        browser = p.chromium.launch(headless=True)
        page = browser.new_page()
        try:
            links = search_document_links(page, query)
            match = pick_best_match(links, query, exact=exact)
            if match is None:
                print(f'Документ по запросу «{query}» не найден.')
                return None
            path = download_file(page, match, target_dir)
            print(f'Скачано: {path}')
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
    return parser.parse_args()


if __name__ == '__main__':
    args = parse_args()
    find_and_download(args.query, target_dir=args.dir, exact=args.exact)
