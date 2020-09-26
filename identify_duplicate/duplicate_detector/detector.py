from git_analysis.analyze_git_logs import retrieve_git_logs
from git_analysis.analyze_git_numstat import get_numstats
from git_analysis.analyze_git_logs import retrieve_git_logs_dict
from git_analysis.analyze_git_logs import get_ancestors
from git_analysis.git_stats.git_numstat import GitNumStat
from diff.diff_get import diff_git

class Detector:
    '''
    该类用于检测重复change，在以git flow模型
    进行工作的项目中，可能有大量重复的变更。
    这些变更就是在不同branch上，修改了相同代码的commit
    如果考虑这些commit，可能会对defect prediction的效果
    可解释性造成影响
    '''

    git_logs = None
    git_numstats = None
    git_log_dict = None

    def __init__(self, log_index, check_ancestor_time_out=360000):
        assert Detector.git_logs is not None
        assert Detector.git_numstats  is not None
        assert Detector.git_log_dict is not None
        self.log_index = log_index
        self.commit_id = Detector.git_logs[log_index].commit_id
        self.check_ancestor_time_out = check_ancestor_time_out
        self.time_stamp = Detector.git_log_dict[self.commit_id].time_stamp
        self.ancestors = get_ancestors(Detector.git_log_dict, self.commit_id,
                                       self.time_stamp, check_ancestor_time_out)
        self.diff=diff_git(Detector.git_log_dict,self.commit_id)
        try:
            self.num_stat = Detector.git_numstats[self.commit_id]
        except KeyError:
            self.num_stat = None

    def retrieve_duplicate_commits(self):
        """
        找到重复的commit
        """
        # 先找时间相似的
        duplicate_commits = list()

        if self.num_stat is None:
            return
        modified_files, renamed_dict = self.num_stat.modified_files
        if len(modified_files) == 0:
            return duplicate_commits

        candidate_commits = list()
        index = self.log_index
        while index >= 0:
            index -= 1
            commit_id2 = Detector.git_logs[index].commit_id#提取出id
            if not self.check_is_recent_commits(commit_id2):
                break
            candidate_commits.append(commit_id2)

        # 然后匹配条件
        for commit_id2 in candidate_commits:
            if self.check_duplicate_commit(commit_id2):
                duplicate_commits.append(commit_id2)
        return duplicate_commits

    def check_different_branches(self, commit_id2):
        """
        如果两个commit互相不是祖先，即id1不是id2的祖先，
        id2不是id1的祖先，那么说明它们在不同branch上，有可能是duplicate
        """
        if commit_id2 in self.ancestors:
            return False
        commit2_time_stamp = Detector.git_log_dict[commit_id2].time_stamp
        ancestors2 = get_ancestors(Detector.git_log_dict, commit_id2,
                                   commit2_time_stamp, self.check_ancestor_time_out)
        if self.commit_id in ancestors2:
            return False
        return True

    def check_same_files(self, commit_id2):
        """
        检查两个commit是否修改了相同的文件，且修改文件行数相同
        addition = addition
        deletion = deletion
        """
        if self.num_stat is None:
            return False

        assert isinstance(self.num_stat, GitNumStat)

        modified_files1, rename_dict1 = self.num_stat.modified_files

        commit2_numstat = Detector.git_numstats[commit_id2]
        assert isinstance(commit2_numstat, GitNumStat)
        modified_files2, rename_dict2 = commit2_numstat.modified_files

        if modified_files1 == modified_files2 and rename_dict1 == rename_dict2:
            #print(modified_files1,rename_dict1)
            return True
        return False

    def check_is_recent_commits(self, commit_id2, time_length=604800):#判断条件改为八小时8*60*60
        """
        检查两个commit是否只有time_length 秒长度
        """
        commit2_time_stamp = Detector.git_log_dict[commit_id2].time_stamp
        return abs(self.time_stamp - commit2_time_stamp) <= time_length


    def check_duplicate_commit(self, commit_id2):
        if self.check_same_files(commit_id2):
            if self.check_different_branches(commit_id2):
                #return True
                if self.check_same_diff(commit_id2):
                    return True
        return False

    # TODO 检查修改代码是否一致
    # 需要比较Git diff
    def check_code_duplicate(self):
        return True

    def check_same_diff(self, commit_id2):
        diff1 = self.diff
        if commit_id2 in self.diff:
            return False
        diff2 = diff_git(Detector.git_log_dict, commit_id2)
        if diff1 == diff2:
            return True
        return False

import csv
def retrieve_duplicate_commits(project):
    """
    获取全部的duplicate change
    """
    # 首先初始化Detector全局变量
    Detector.git_logs = retrieve_git_logs(project)
    # 确保git_log 按照时间顺序
    Detector.git_logs.sort(key=lambda x: x.time_stamp, reverse=True)
    Detector.git_log_dict = retrieve_git_logs_dict(project)
    Detector.git_numstats = get_numstats(project)

    # 对每个commit进行处理
    index = 0
    while index < len(Detector.git_logs):
        print(index)
        detector = Detector(index)
        dup_commits = detector.retrieve_duplicate_commits()
        if len(dup_commits) > 0:
            print("FIND DUPLICATE")
            print(detector.commit_id)
            # print(type(detector.commit_id))
            # with open('E:\\git-data\dup-days\\more-hbase_id.csv','a+') as f:
            #     cw=csv.writer(f,lineterminator='\n')
            #     cw.writerow([detector.commit_id])
            print(dup_commits)
            with open('E:\\git-data\dup-days\\i-superset.csv','a+') as f:
                cw=csv.writer(f,lineterminator='\n')
                cw.writerow(dup_commits)

        index += 1

if __name__ == '__main__':
    retrieve_duplicate_commits('i-superset')
#spring 11603 dup:381
#django 无
#ant 16878 362
#raiden 无
#i-superset 无
