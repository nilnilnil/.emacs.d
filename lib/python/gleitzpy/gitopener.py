import sys
import os

def _get_os_result(command):
    return os.popen(command).readline().strip()

def get_git_root_directory(path):
    if not path or len(path) == 1:
        return False
    dir_name = os.path.dirname(path)
    git_path = os.path.join(dir_name, '.git')
    if os.path.exists(git_path):
        return dir_name
    return get_git_root_directory(dir_name)

def get_github_address():
    url = _get_os_result('git config --get remote.origin.url')
    url = url[url.find('@') + 1:url.rfind('.git')]
    url = url.replace(':', '/')
    return url

def get_branch_name():
    return _get_os_result('git rev-parse --abbrev-ref HEAD')

def get_github_url(filename):
    github_address = get_github_address()
    branch_name = get_branch_name()
    git_root_directory = get_git_root_directory(filename)
    if not all([github_address, branch_name, git_root_directory]):
        return False
    relative_path = filename.replace(git_root_directory, '')
    if os.path.isdir(filename):
        url_str = 'http://{0}/tree/{1}{2}'
    else:
        url_str = 'http://{0}/blob/{1}{2}'
    complete_url = url_str.format(github_address,
                                  branch_name,
                                  relative_path)
    return complete_url


def main():
    if len(sys.argv) > 1:
        filename = sys.argv[1]
    else:
        filename = os.getcwd()
    if os.path.isdir(filename):
        os.chdir(filename)
    else:
        os.chdir(os.path.dirname(filename))
    print get_github_url(filename)

if __name__ == '__main__':
    main()
