

def read_duplicate_commits_from_csv(csv_path):
    ret = set()
    with open(csv_path, 'r') as csv_file:
        lines = csv_file.readlines()
        for line in lines:
            commit_ids = line.split(',')
            for commit_id in commit_ids:
                tmp_commit_id = commit_id.strip()
                if tmp_commit_id != '':
                    ret.add(tmp_commit_id)
    return ret


if __name__ == '__main__':
    csv_path = 'H://Paper/duplicate changes/duplicate commit/week-activemq.csv'
    ret = read_duplicate_commits_from_csv(csv_path)
    print(ret)